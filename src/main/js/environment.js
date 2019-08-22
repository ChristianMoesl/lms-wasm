const fs = require('fs');

function printUsage() {
    /*PRINT_USAGE*/
    process.exit(1);
}

if (process.argv.length !== 3)
    printUsage();

/*PARSE_ARGS*/

/*CHECK_ARGS*/ printUsage();

const binaryPath = process.argv[1].replace(".js", ".wasm");

const wasm = new Uint8Array(fs.readFileSync(binaryPath));

const memory = new WebAssembly.Memory({ initial: 256, maximum: 256 });

var mem = null;

function strlen(arr) {
    var len = 0;
    while (arr[len] !== 0) { len++; }
    return len;
}

function uint8ToString(arr){
    return String.fromCharCode.apply(null, arr);
}

function insertAt(arr, idx, string) {
    for (var i = 0; i < string.length; i++)
        arr[idx++] = string.charCodeAt(i);
    arr[idx++] = 0;
    return idx;
}

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
    printlnString: x => {
        const buf = new Uint8Array(mem.buffer, x);
        const len = strlen(buf);
        console.log(uint8ToString(buf.subarray(0, len)));
    },
    printlnBoolean: x => console.log(x !== 0),
    exit: status => process.exit(status)
};

WebAssembly.instantiate(wasm, { env })
    .then(results => {
        mem = results.instance.exports.mem;
        /*INSERT_STRINGS*/
        const returned = results.instance.exports/*FUNCTION_CALL*/;
        if (typeof(returned) != 'undefined')
            console.log(returned);
    }).catch(reason => {
    console.error(reason);
    process.error(1);
});