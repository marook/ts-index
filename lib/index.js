// ts-index - a fast typescript artifact index
// Copyright (C) 2021  Markus Peröbner
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

let chokidar = require('chokidar');
let fs = require('fs/promises');
let glob = require('glob');
let path = require('path');
let ts = require('typescript');
let yargs = require('yargs/yargs');
let { hideBin } = require('yargs/helpers');
let argParser = yargs(hideBin(process.argv))
    .option('single', {
        alias: 's',
        type: 'boolean',
        description: 'Performs only a single scan instead of watching the file system for changes.',
    })
    .option('log', {
        type: 'string',
        default: 'ts-index.log',
        description: 'Errors and warnings during the parsing process are written to this log file.',
    });
let argv = argParser.argv;

let log = require('simple-node-logger').createSimpleFileLogger(argv.log);

let sk = ts.SyntaxKind;

let ignoredSyntaxKinds = new Set([
    sk.Block,
    sk.DeclareKeyword,
    sk.DoStatement,
    sk.EmptyStatement,
    sk.EndOfFileToken,
    sk.ExportAssignment,
    sk.ExportDeclaration,
    sk.ExpressionStatement,
    sk.ForInStatement,
    sk.IfStatement,
    sk.ImportDeclaration,
    sk.LabeledStatement,
    sk.MissingDeclaration,
    sk.WithStatement,
    sk.WhileStatement,
]);

let exportModifierNodeType = new Map([
    [sk.ClassDeclaration, 'class'],
    [sk.EnumDeclaration, 'enum'],
    [sk.FunctionDeclaration, 'function'],
    [sk.InterfaceDeclaration, 'interface'],
    [sk.ModuleDeclaration, 'module'],
    [sk.TypeAliasDeclaration, 'type'],
]);

if(argv._.length === 0){
    console.error('Missing project root directory argument.');
    argParser.showHelp();
    process.exitCode = 1;
    process.exit();
}

let rootPath = argv._[0];

if(argv.single){
    scanRootDir();
} else {
    watchRootDir();
}

function scanRootDir(){
    glob(getRootPathGlob(), async (e, files) => {
        for(let f of files){
            if(isPathIgnored(f)){
                continue;
            }
            await addFile(f);
        }
    });
}

function watchRootDir(){
    chokidar
        .watch(getRootPathGlob(), {
            ignored: isPathIgnored,
        })
        .on('all', async (event, changedPath) => {
            try {
                if(event === 'add'){
                    await addFile(changedPath);
                } else if(event === 'change') {
                    removeFile(changedPath);
                    await addFile(changedPath);
                } else if(event === 'unlink') {
                    removeFile(changedPath);
                } else if (event === 'addDir' || event === 'unlinkDir'){
                    // do nothing
                } else {
                    log.warn('Unknown event: ', event);
                }
            } catch (e) {
                log.error(`Failed to process ${changedPath}: `, e);
            }
        })
        .on('error', e => {
            log.error(e);
        });
}

function getRootPathGlob(){
    return path.join(rootPath, '**', '*.ts');
}

function isPathIgnored(p){
    if(p.lastIndexOf(`node_modules${path.sep}`) !== -1){
        return true;
    }
    if(p.indexOf(`${path.sep}node_modules${path.sep}`) !== -1){
        return true;
    }
    return false;
}

async function addFile(filePath){
    let [relFilePath, workspacePath] = detectWorkspace(filePath);
    let fileGlobals = await parseGlobals(relFilePath, workspacePath);
    for(let g of fileGlobals){
        console.log(`("+" ${quote(filePath)} ${quote(g.type)} ${quote(g.name)} ${g.exported ? 't' : 'nil'} ${g.pos})`);
    }
}

function removeFile(filePath){
    console.log(`("-" ${quote(filePath)})`);
}

function quote(s){
    return JSON.stringify(s || '');
}

function detectWorkspace(filePath){
    let pathItems = filePath.split(path.sep);
    let srcPos = pathItems.lastIndexOf('src');
    if(srcPos === -1){
        throw new Error('Paths without a src directory are unsupported right now');
    }
    return [
        pathItems.slice(srcPos + 1).join(path.sep),
        pathItems.slice(0, srcPos + 1).join(path.sep),
    ];
}

/**
 * parseExports parses the given typescript file and returns globals.
 *
 * tsFilePath must be a path relative to workspacePath.
 *
 * Returns an array of global artifacts. Each global artifact has the
 * following properties:
 * - name is the name of the artifact in the global namespace.
 * - type is the type of the artifact. Possible types are 'class'.
 * - exported is true if the global artifact is exported by the
 *   typescript file.
 */
async function parseGlobals(tsFilePath, workspacePath){
    let code = await fs.readFile(path.join(workspacePath, tsFilePath), {
        encoding: 'utf-8',
    });
    let globals = [];
    let sourceFile = ts.createSourceFile(tsFilePath, code);
    try {
        appendGlobals(globals, sourceFile);
    } catch(e) {
        throw new Error(`Unable to parse globals in file ${tsFilePath}: ${e}`);
    }
    return globals;
}

function appendGlobals(globals, node){
    ts.forEachChild(node, node => {
        if(ignoredSyntaxKinds.has(node.kind)){
            return;
        }
        if(node.kind === ts.SyntaxKind.VariableStatement){
            let exported = hasExportStatement(node);
            ts.forEachChild(node, node => {
                if(node.kind === ts.SyntaxKind.VariableDeclarationList){
                    node.declarations.forEach(d => {
                        if(d.kind === ts.SyntaxKind.VariableDeclaration){
                            appendGlobal('variable', d, exported);
                        }
                        else {
                            throw new Error(`Unknown SyntaxKind in VariableDeclarationList ${node.kind}`);
                        }
                    });
                } else if (node.kind === ts.SyntaxKind.ExportKeyword) {
                } else {
                    throw new Error(`Unknown SyntaxKind in VariableStatement ${node.kind}`);
                }
            });
        } else if(exportModifierNodeType.has(node.kind)) {
            let type = exportModifierNodeType.get(node.kind);
            appendGlobal(type, node, hasExportModifier(node));
        } else {
            throw new Error(`Unknown SyntaxKind with value ${node.kind}: ${JSON.stringify(node)}`);
        }

        function appendGlobal(type, node, exported=false){
            globals.push({
                type,
                name: getNodeName(node),
                exported,
                pos: node.name.pos,
            });
        }
});

}

function getNodeName(node){
    return node.name.text || node.name.escapedText;
}

function hasExportModifier(node){
    if(node.modifiers){
        for(let m of node.modifiers){
            if(m.kind === ts.SyntaxKind.ExportKeyword){
                return true;
            }
        }
    }
    return false;
}

function hasExportStatement(node){
    let exported = false;
    ts.forEachChild(node, child => {
        if(child.kind === ts.SyntaxKind.ExportKeyword){
            exported = true;
        }
    });
    return exported;
}
