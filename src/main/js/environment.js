const fs = require('fs');
const printf = require('printf');

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

const heapInGb = 4; // 4 is maximum for wasm32
const bytesPerPage = 64 * 1024;

var mem = null;

var allocated = 0;
var bump = 0;

function malloc(size) {
    // for word alignment
    const alignedSize = Math.ceil(size / 8.0) * 8;

    if (alignedSize > allocated - bump) {
        allocated = mem.grow(Math.ceil(alignedSize / bytesPerPage)) * bytesPerPage;

        if (bump === 0) bump = allocated;
    }

    const addr = bump;

    bump = bump + alignedSize;

    return addr;
}

function strlen(arr) {
    var len = 0;
    while (arr[len] !== 0) { len++; }
    return len;
}

function str(ptr, len) {
    const buf = new Uint8Array(mem.buffer, ptr);
    const length = (len == null) ? strlen(buf) : len;
    return String.fromCharCode.apply(null, (buf.subarray(0, length)));
}

function save(str) {
    const addr = malloc(str.length + 1);
    insertAt(new Uint8Array(mem.buffer), addr, str);
    return addr;
}

function insertAt(arr, idx, string) {
    for (var i = 0; i < string.length; i++)
        arr[idx++] = string.charCodeAt(i);
    arr[idx++] = 0;
    return idx;
}

function loadArgs(typeMask) {
    var args = [];
    for (var i = 0; i < 4; i++) {
        switch ((typeMask >> (i * 8)) & 255) {
            case 0: // no argument
                return args;
            case 1: // boolean
                args.push(new Uint32Array(mem.buffer)[i * 2]);
                break;
            case 2: // char
                args.push(String.fromCharCode(new Uint32Array(mem.buffer)[i * 2]));
                break;
            case 3: // string
                args.push(str(new Uint32Array(mem.buffer)[i * 2]));
                break;
            case 4: // int
                args.push(Number(new BigUint64Array(mem.buffer)[i]));
                break;
            case 5: // long
                args.push(Number(new BigUint64Array(mem.buffer)[i]));
                break;
            case 6: // float
                args.push(new Float32Array(mem.buffer)[i * 2]);
                break;
            case 7: // double
                args.push(new Float64Array(mem.buffer)[i]);
                break;
            default:
                throw new Error("Type not implemented");
        }
    }
    return args;
}

function println(typeMask) {
    loadArgs(typeMask).forEach(x => console.log(x));
}

const memory = new WebAssembly.Memory({
    initial: 256,
    maximum: heapInGb * Math.pow(2, 30) / bytesPerPage
});

const env = {
    abortStackOverflow: (err) => { throw new Error(`overflow: ` + err); },
    table: new WebAssembly.Table({ initial: 0, maximum: 0, element: 'anyfunc' }),
    __table_base: 0,
    memory,
    __memory_base: 1024,
    STACKTOP: 0,
    STACK_MAX: memory.buffer.byteLength,
    malloc: x => malloc(x),
    println0: () => console.log(),
    println1: (typeMask) => println(typeMask),
    println2: (typeMask) => println(typeMask),
    println3: (typeMask) => println(typeMask),
    println4: (typeMask) => println(typeMask),
    printf0: (f) => process.stdout.write(str(f)),
    printf1: (f, typeMask) => printf(process.stdout, str(f), ...loadArgs(typeMask)),
    printf2: (f, typeMask) => printf(process.stdout, str(f), ...loadArgs(typeMask)),
    printf3: (f, typeMask) => printf(process.stdout, str(f), ...loadArgs(typeMask)),
    printf4: (f, typeMask) => printf(process.stdout, str(f), ...loadArgs(typeMask)),
    printData: (start, len) => new Uint8Array(mem.buffer, start, len).forEach(x => process.stdout.write(String.fromCharCode(x))),
    stringSlice: (s, start, end) => save(str(s).slice(start, end)),
    stringToDouble: s => Number.parseFloat(str(s)),
    stringToInt: s => Number.parseInt(str(s)),
    stringLength: s => str(s).length,
    stringCharAt: (s, i) => str(s).charAt(i),
    exit: status => process.exit(status),
    open: name => fs.openSync(str(name), 'r'),
    close: fd => fs.closeSync(fd),
    readFile: name => {
        const path = str(name);
        const len = fs.statSync(path).size;
        const start = malloc(len);
        new Uint32Array(mem.buffer, start).fill(len, 0, 1);
        const fd = fs.openSync(path, 0);
        fs.readSync(fd, new Uint8Array(mem.buffer, start), 4, len, null);
        return start + 4;
    },
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