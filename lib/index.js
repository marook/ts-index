let chokidar = require('chokidar');
let fs = require('fs/promises');
let path = require('path');
let ts = require('typescript');

let log = require('simple-node-logger').createSimpleFileLogger('ts-index.log');

let ignoredSyntaxKinds = new Set([
    ts.SyntaxKind.DeclareKeyword,
    ts.SyntaxKind.EndOfFileToken,
    ts.SyntaxKind.ExpressionStatement,
    ts.SyntaxKind.IfStatement,
    ts.SyntaxKind.ImportDeclaration,
]);

let rootPath = process.argv[2];

chokidar
    .watch(rootPath)
    .on('all', async (event, changedPath) => {
        try {
            if(!changedPath.endsWith('.ts')){
                return;
            }
            if(event === 'add'){
                await addFile(changedPath);
            } else if(event === 'change') {
                removeFile(changedPath);
                await addFile(changedPath);
            } else if(event === 'unlink') {
                removeFile(changedPath);
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

async function addFile(filePath){
    let [relFilePath, workspacePath] = detectWorkspace(filePath);
    let fileGlobals = await parseGlobals(relFilePath, workspacePath);
    for(let g of fileGlobals){
        console.log(`("+" "${filePath}" "${g.type}" "${g.name}" ${g.exported ? 't' : 'nil'} ${g.pos})`);
    }
}

function removeFile(filePath){
    console.log(`("-" "${filePath}")`);
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
    appendGlobals(globals, sourceFile);
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
        } else if (node.kind === ts.SyntaxKind.TypeAliasDeclaration) {
            appendGlobal('type', node, hasExportModifier(node));
        } else if (node.kind === ts.SyntaxKind.ClassDeclaration) {
            appendGlobal('class', node, hasExportModifier(node));
        } else if (node.kind === ts.SyntaxKind.InterfaceDeclaration) {
            appendGlobal('interface', node, hasExportModifier(node));
        } else if (node.kind === ts.SyntaxKind.FunctionDeclaration) {
            appendGlobal('function', node, hasExportModifier(node));
        } else if (node.kind === ts.SyntaxKind.ModuleDeclaration) {
            appendGlobal('module', node, hasExportModifier(node));
        } else if (node.kind === ts.SyntaxKind.EnumDeclaration) {
            appendGlobal('enum', node, hasExportModifier(node));
        } else {
            throw new Error(`Unknown SyntaxKind with value ${node.kind}`);
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
