const fs = require('fs');

function printUsage() {
    /*PRINT_USAGE*/
    process.exit(1);
}

if (process.argv.length != 3)
    printUsage();

/*PARSE_ARGS*/

/*CHECK_ARGS*/ printUsage();

const binaryPath = process.argv[1].replace(".js", ".wasm");

const wasm = new Uint8Array(fs.readFileSync(binaryPath));

const memory = new WebAssembly.Memory({ initial: 256, maximum: 256 });

const env = {
    abortStackOverflow: (err) => { throw new Error(`overflow: ` + err); },
    table: new WebAssembly.Table({ initial: 0, maximum: 0, element: 'anyfunc' }),
    __table_base: 0,
    memory,
    __memory_base: 1024,
    STACKTOP: 0,
    STACK_MAX: memory.buffer.byteLength,
    printlnInt: x => console.log(x),
    printlnFloat: x => console.log(x),
    printlnString: x => console.log(x),
    printlnBoolean: x => console.log(x !== 0),
    exit: status => process.exit(status)
};

WebAssembly.instantiate(wasm, { env })
    .then(results => {
        const returned = results.instance.exports/*FUNCTION_CALL*/;
        if (typeof(returned) != 'undefined')
            console.log(returned);
    }).catch(reason => {
    console.error(reason);
    process.error(1);
});