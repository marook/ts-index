export function myExportedFunction(){}

export interface SomeExportedInterface {
    data: number;
}

export class SomeExportedClass {
    field: string;
}

class SomeNotExportedClass {
    f(){}
}
