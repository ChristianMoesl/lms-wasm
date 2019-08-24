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

function str(ptr, len) {
    const buf = new Uint8Array(mem.buffer, ptr);
    const length = (len == null) ? strlen(buf) : len;
    return uint8ToString(buf.subarray(0, length));
}

function save(str, addr) {
    insertAt(new Uint8Array(mem.buffer), addr, str);
}

function saveNew(str) {
    const addr = mem.grow(str.length + 1);
    save(str, addr);
    return addr;
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
    malloc: x => mem.grow(x),
    printlnInt: x => console.log(x),
    printInt: x => process.stdout.write(x.toString()),
    printlnFloat: x => console.log(x),
    printlnString: ptr => console.log(str(ptr)),
    printString: s => process.stdout.write(str(s)),
    printlnBoolean: x => console.log(x !== 0),
    printlnChar: c => console.log(String.fromCharCode(c)),
    stringSlice: (s, start, end) => saveNew(str(s).slice(start, end)),
    // stringEqualsTo: (ls, rs) => str(ls) === str(rs),
    stringToDouble: s => Number.parseFloat(str(s)),
    stringToInt: s => Number.parseInt(str(s)),
    stringLength: s => str(s).length,
    stringCharAt: (s, i) => str(s).charAt(i),
    exit: status => process.exit(status),
    open: name => fs.openSync(str(name), 'r'),
    close: fd => fs.closeSync(fd),
    fsize: fd => fs.fstatSync(fd).size,
    mmap: (fd, len) => {
        const bytesPerPage = 64 * 1024;
        const start = bytesPerPage * mem.grow(Math.ceil((len * 1.0) / bytesPerPage));
        fs.readSync(fd, new Uint8Array(mem.buffer), start, len, null);
        return start;
    },
    stringFromCharArray: (arr, pos, len) => saveNew(str(arr + pos, len)),
    printll: s => {
        const arr = new Uint8Array(mem.buffer);
        while (arr[s] !== '\n' && arr[s] !== ',' && arr[s] !== '\t' && arr[s] !== 0)
            process.stdout.write(String.fromCharCode(arr[s++]));
        return 0;
    }
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
    process.exit(1);
});