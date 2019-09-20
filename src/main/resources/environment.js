const isBrowser = typeof window !== 'undefined' && typeof window.document !== 'undefined';
let   isNode = typeof process !== 'undefined' && process.versions != null && process.versions.node != null;
const isLambda = isNode && !!(process.env.LAMBDA_TASK_ROOT || false);
if (isLambda) isNode = false;

const fs = (isNode  || isLambda) ? require('fs') : null;
const util = (isNode || isLambda) ? require('util') : null;
const printf = require('printf');

const readFile = (isNode || isLambda) ? util.promisify(fs.readFile) : null;

let consoleOutput = [ ];

function getBinaryPathInBrowser() {
    const x = document.currentScript.src.split("/");
    return x[x.length - 1].replace(".js", ".wasm");
}

function getBinaryPathInNode() {
    return process.argv[1].replace(".js", ".wasm");
}

const wasmPath = isLambda ? "index.wasm" : isBrowser ? getBinaryPathInBrowser() : getBinaryPathInNode();

const heapInGb = 4; // 4 is maximum for wasm32
const bytesPerPage = 64 * 1024;

let mem = null;

let allocated = 0;
let bump = 0;

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
    let len = 0;
    while (arr[len] !== 0) { len++; }
    return len;
}

function str(ptr, len) {
    const buf = new Uint8Array(mem.buffer, ptr);
    const length = (len == null) ? strlen(buf) : len;
    return String.fromCharCode.apply(null, (buf.subarray(0, length)));
}

function insertAt(arr, idx, string) {
    for (let i = 0; i < string.length; i++)
        arr[idx++] = string.charCodeAt(i);
    arr[idx++] = 0;
    return idx;
}

function save(str) {
    const addr = malloc(str.length + 1);
    insertAt(new Uint8Array(mem.buffer), addr, str);
    return addr;
}

function loadArgs(typeMask) {
    let args = [];
    for (let i = 0; i < 4; i++) {
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

function printlnConsole(typeMask) {
    if (typeMask === 0)
        console.log();
    else
        loadArgs(typeMask).forEach(x => console.log(x));
}

function printlnString(typeMask) {
    if (typeMask === 0)
        consoleOutput.push("\n");
    else
        loadArgs(typeMask).forEach(x => consoleOutput.push([x, "\n"]));
}

function printfConsole(formatString, typeMask) {
    if (typeMask == null)
        process.stdout.write(str(formatString));
    else
        printf(process.stdout, str(formatString), ...loadArgs(typeMask));
}

function printfString(formatString, typeMask) {
    consoleOutput.push(typeMask == null ? str(formatString) : printf(str(formatString), ...loadArgs(typeMask)));
}

function printDataConsole(start, len) {
    new Uint8Array(mem.buffer, start, len).forEach(x => process.stdout.write(String.fromCharCode(x)))
}

function printDataString(start, len) {
    consoleOutput.push(str(start, len));
}

function readFileNode(name) {
    const path = str(name);
    const len = fs.statSync(path).size;
    const start = malloc(len + 4);
    new Uint32Array(mem.buffer, start).fill(len, 0, 1);
    const fd = fs.openSync(path, 0);
    fs.readSync(fd, new Uint8Array(mem.buffer, start), 4, len, null);
    return start + 4;
}

function fetchFile(name) {
    if ((typeof files) === "undefined" || files.get(str(name)) === undefined)
        error("file has to be prefetched in browser");

    const file = new Uint8Array(files.get(str(name)));

    const len = file.length;
    const start = malloc(len + 4);
    new Uint32Array(mem.buffer, start).fill(len, 0, 1);
    new Uint8Array(mem.buffer, start + 4).set(file);
    return start + 4;
}

function error(string) {
    if (isNode) {
        console.error(string);
        process.exit(1);
    } else
        throw Error(string);
}

async function fetchBinary(path) {
    let binary = null;
    if (isBrowser) {
        const response = await fetch(path);
        binary = await response.arrayBuffer();
    } else
        binary = await readFile(path);
    return new Uint8Array(binary);
}

async function run(...args) {
    function printUsage() {
        /*USAGE_STRING*/
        error(s);
    }

    /*PARSE_ARGS*/

    /*CHECK_ARGS*/ printUsage();

    consoleOutput = [ ];

    allocated = 0;
    bump = 0;

    const memory = new WebAssembly.Memory({
        initial: 256,
        maximum: heapInGb * Math.pow(2, 30) / bytesPerPage
    });

    let env = {
        abortStackOverflow: (err) => { throw new Error(`overflow: ` + err); },
        table: new WebAssembly.Table({ initial: 0, maximum: 0, element: 'anyfunc' }),
        __table_base: 0,
        memory,
        __memory_base: 1024,
        STACKTOP: 0,
        STACK_MAX: memory.buffer.byteLength,
        malloc: x => malloc(x),
        println: isNode ? printlnConsole : printlnString,
        printf: isNode ? printfConsole : printfString,
        printData: isNode ? printDataConsole : printDataString,
        stringSlice: (s, start, end) => save(str(s).slice(start, end)),
        stringToDouble: s => Number.parseFloat(str(s)),
        stringToInt: s => Number.parseInt(str(s)),
        stringLength: s => str(s).length,
        stringCharAt: (s, i) => str(s).charAt(i),
        readFile: isBrowser ? fetchFile : readFileNode,
    };

    let returned = undefined;
    try {
        const wasm = await fetchBinary(wasmPath);
        const results = await WebAssembly.instantiate(wasm, { env });

        mem = results.instance.exports.mem;
        /*INSERT_STRINGS*/
        returned = results.instance.exports/*FUNCTION_CALL*/;

    } catch (reason) {
        error(reason.toString());
    }

    return {
        status: returned,
        output: consoleOutput.join(""),
    };
}

async function runBinary(binary) {
    consoleOutput = [ ];

    allocated = 0;
    bump = 0;

    const memory = new WebAssembly.Memory({
        initial: 256,
        maximum: heapInGb * Math.pow(2, 30) / bytesPerPage
    });

    let env = {
        abortStackOverflow: (err) => { throw new Error(`overflow: ` + err); },
        table: new WebAssembly.Table({ initial: 0, maximum: 0, element: 'anyfunc' }),
        __table_base: 0,
        memory,
        __memory_base: 1024,
        STACKTOP: 0,
        STACK_MAX: memory.buffer.byteLength,
        malloc: x => malloc(x),
        println0: isNode ? printlnConsole : printlnString,
        println1: isNode ? printlnConsole : printlnString,
        println2: isNode ? printlnConsole : printlnString,
        println3: isNode ? printlnConsole : printlnString,
        println4: isNode ? printlnConsole : printlnString,
        printf0: isNode ? printfConsole : printfString,
        printf1: isNode ? printfConsole : printfString,
        printf2: isNode ? printfConsole : printfString,
        printf3: isNode ? printfConsole : printfString,
        printf4: isNode ? printfConsole : printfString,
        printData: isNode ? printDataConsole : printDataString,
        stringSlice: (s, start, end) => save(str(s).slice(start, end)),
        stringToDouble: s => Number.parseFloat(str(s)),
        stringToInt: s => Number.parseInt(str(s)),
        stringLength: s => str(s).length,
        stringCharAt: (s, i) => str(s).charAt(i),
        readFile: isBrowser ? fetchFile : readFileNode,
    };

    let returned = undefined;
    try {
        const results = await WebAssembly.instantiate(binary, { env });

        mem = results.instance.exports.mem;
        returned = results.instance.exports.Snippet(1);

    } catch (reason) {
        error(reason.toString());
    }

    return {
        status: returned,
        output: consoleOutput.join(""),
    };
}

if (isNode) run(...process.argv.slice(2));

if (isLambda) {
    exports.handler = async (event, context, callback) => {
        context.succeed((await run("a")).output);
    };
}

if (isBrowser) window.runWasm = run;
