const { display } = require('sicp')

Object.entries(require('sicp')).forEach(
  ([name, exported]) => (global[name] = exported)
)

const push = (array, ...items) => {
  // fixed by Liew Zhao Wei, see Discussion 5
  for (let item of items) {
    array.push(item)
  }
  return array
}

// return the last element of given array
// without changing the array
const peek = (array, address) => array.slice(-1 - address)[0]

// *************
// parse to JSON
// *************/

const list_to_array = (xs) =>
  is_null(xs) ? [] : [head(xs)].concat(list_to_array(tail(xs)))

// simplify parameter format
const parameters = (xs) => map((x) => head(tail(x)), xs)

// turn tagged list syntax from parse into JSON object
const ast_to_json = (t) => {
  switch (head(t)) {
    case 'literal':
      return { tag: 'lit', val: head(tail(t)) }
    case 'name':
      return { tag: 'nam', sym: head(tail(t)) }
    case 'application':
      return {
        tag: 'app',
        fun: ast_to_json(head(tail(t))),
        args: list_to_array(map(ast_to_json, head(tail(tail(t))))),
      }
    case 'logical_composition':
      return {
        tag: 'log',
        sym: head(tail(t)),
        frst: ast_to_json(head(tail(tail(t)))),
        scnd: ast_to_json(head(tail(tail(tail(t))))),
      }
    case 'binary_operator_combination':
      return {
        tag: 'binop',
        sym: head(tail(t)),
        frst: ast_to_json(head(tail(tail(t)))),
        scnd: ast_to_json(head(tail(tail(tail(t))))),
      }
    case 'unary_operator_combination':
      return {
        tag: 'unop',
        sym: head(tail(t)),
        frst: ast_to_json(head(tail(tail(t)))),
      }
    case 'lambda_expression':
      return {
        tag: 'lam',
        prms: list_to_array(parameters(head(tail(t)))),
        body: ast_to_json(head(tail(tail(t)))),
      }
    case 'sequence':
      return {
        tag: 'seq',
        stmts: list_to_array(map(ast_to_json, head(tail(t)))),
      }
    case 'block':
      return {
        tag: 'blk',
        body: ast_to_json(head(tail(t))),
      }
    case 'variable_declaration':
      return {
        tag: 'let',
        sym: head(tail(head(tail(t)))),
        expr: ast_to_json(head(tail(tail(t)))),
      }
    case 'constant_declaration':
      return {
        tag: 'const',
        sym: head(tail(head(tail(t)))),
        expr: ast_to_json(head(tail(tail(t)))),
      }
    case 'assignment':
      return {
        tag: 'assmt',
        sym: head(tail(head(tail(t)))),
        expr: ast_to_json(head(tail(tail(t)))),
      }
    case 'conditional_statement':
      return {
        tag: 'cond', // dont distinguish stmt and expr
        pred: ast_to_json(head(tail(t))),
        cons: ast_to_json(head(tail(tail(t)))),
        alt: ast_to_json(head(tail(tail(tail(t))))),
      }
    case 'conditional_expression':
      return {
        tag: 'cond', // dont distinguish stmt and expr
        pred: ast_to_json(head(tail(t))),
        cons: ast_to_json(head(tail(tail(t)))),
        alt: ast_to_json(head(tail(tail(tail(t))))),
      }
    case 'function_declaration':
      return {
        tag: 'fun',
        sym: head(tail(head(tail(t)))),
        prms: list_to_array(parameters(head(tail(tail(t))))),
        body: ast_to_json(head(tail(tail(tail(t))))),
      }
    case 'return_statement':
      return {
        tag: 'ret',
        expr: ast_to_json(head(tail(t))),
      }
    case 'while_loop':
      return {
        tag: 'while',
        pred: ast_to_json(head(tail(t))),
        body: ast_to_json(head(tail(tail(t)))),
      }
    default:
      error(t, 'unknown syntax:')
  }
}

// parse, turn into json (using ast_to_json),
// and wrap in a block
const parse_to_json = (program_text) => ({
  tag: 'blk',
  body: ast_to_json(parse(program_text)),
})

// *************************
// HEAP
// *************************/

// HEAP is an array of bytes (JS ArrayBuffer)

const word_size = 8

// heap_make allocates a heap of given size
// (in bytes) and returns a DataView of that,
// see https://www.javascripture.com/DataView
const heap_make = (words) => {
  const data = new ArrayBuffer(words * word_size)
  const view = new DataView(data)
  return view
}

// for convenience, HEAP is global variable
// initialized in initialize_machine()
let HEAP
let heap_size

// free is the next free index in the free list
let free

// for debugging: display all bits of the heap
const heap_display = (s) => {
  display('', 'heap: ' + s)
  for (let i = 0; i < heap_size; i++) {
    display(
      word_to_string(heap_get(i)),
      stringify(i) + ' ' + stringify(heap_get(i)) + ' '
    )
  }
}

// heap_allocate allocates a given number of words
// on the heap and marks the first word with a 1-byte tag.
// the last two bytes of the first word indicate the number
// of children (addresses) that follow the tag word:
// [1 byte tag, 4 bytes payload (depending on node type),
//  2 bytes #children, 1 byte unused]
// Note: payload depends on the type of node
const size_offset = 5

const node_size = 100

const heap_allocate = (tag, size) => {
  if (size > node_size) {
    error('limitation: nodes cannot be larger than 10 words')
  }
  // a value of -1 in free indicates the
  // end of the free list
  if (free === -1) {
    mark_sweep()
  }

  // allocate
  const address = free
  free = heap_get(free)
  HEAP.setInt8(address * word_size, tag)
  HEAP.setUint16(address * word_size + size_offset, size)
  return address
}

// modified
const mark_bit = 7

const UNMARKED = 0
const MARKED = 1

let HEAP_BOTTOM
let ALLOCATING

const mark_sweep = () => {
  // mark r for r in roots
  const roots = [...OS, E, ...RTS, ...ALLOCATING]
  for (let i = 0; i < roots.length; i++) {
    mark(roots[i])
  }

  sweep()

  if (free === -1) {
    error('heap memory exhausted')
    // or error("out of memory")
  }
}

const mark = (node) => {
  if (node >= heap_size) {
    return
  }

  if (is_unmarked(node)) {
    heap_set_byte_at_offset(node, mark_bit, MARKED)

    const num_of_children = heap_get_number_of_children(node)

    for (let i = 0; i < num_of_children; i++) {
      mark(heap_get_child(node, i))
    }
  }
}

const sweep = () => {
  let v = HEAP_BOTTOM

  while (v < heap_size) {
    if (is_unmarked(v)) {
      free_node(v)
    } else {
      heap_set_byte_at_offset(v, mark_bit, UNMARKED)
    }

    v = v + node_size
  }
}

const is_unmarked = (node) =>
  heap_get_byte_at_offset(node, mark_bit) === UNMARKED

const free_node = (node) => {
  // heap set is used for retrieving the next free node
  heap_set(node, free)
  free = node
}

const heap_already_copied = (node) =>
  heap_get_forwarding_address(node) >= to_space &&
  heap_get_forwarding_address(node) <= free

const heap_set_forwarding_address = (node, address) =>
  HEAP.setInt32(node * word_size, address)

const heap_get_forwarding_address = (node) => HEAP.getInt32(node * word_size)

// get and set a word in heap at given address
const heap_get = (address) => HEAP.getFloat64(address * word_size)

const heap_set = (address, x) => {
 HEAP.setFloat64(address * word_size, x)}

// child index starts at 0
const heap_get_child = (address, child_index) =>
  heap_get(address + 1 + child_index)

const heap_set_child = (address, child_index, value) =>
  heap_set(address + 1 + child_index, value)

const heap_get_tag = (address) => HEAP.getInt8(address * word_size)

const heap_get_size = (address) =>
  HEAP.getUint16(address * word_size + size_offset)

// the number of children is one less than the size
// except for number nodes:
//                 they have size 2 but no children
const heap_get_number_of_children = (address) =>
  heap_get_tag(address) === Number_tag ? 0 : heap_get_size(address) - 1

// access byte in heap, using address and offset
const heap_set_byte_at_offset = (address, offset, value) =>
  HEAP.setUint8(address * word_size + offset, value)

const heap_get_byte_at_offset = (address, offset) =>
  HEAP.getUint8(address * word_size + offset)

// access byte in heap, using address and offset
const heap_set_2_bytes_at_offset = (address, offset, value) =>
  HEAP.setUint16(address * word_size + offset, value)

const heap_get_2_bytes_at_offset = (address, offset) =>
  HEAP.getUint16(address * word_size + offset)

 // ADDED CHANGE
const heap_set_4_bytes_at_offset = (address, offset, value) =>
  HEAP.setUint32(address * word_size + offset, value)

// ADDED CHANGE
const heap_get_4_bytes_at_offset = (address, offset, value) =>
  HEAP.getUint32(address * word_size + offset)

// for debugging: return a string that shows the bits
// of a given word
const word_to_string = (word) => {
  const buf = new ArrayBuffer(8)
  const view = new DataView(buf)
  view.setFloat64(0, word)
  let binStr = ''
  for (let i = 0; i < 8; i++) {
    binStr += ('00000000' + view.getUint8(i).toString(2)).slice(-8) + ' '
  }
  return binStr
}

// values

// All values are allocated on the heap as nodes. The first
// word of the node is a header, and the first byte of the
// header is a tag that identifies the type of node

// a little trick: tags are all negative so that we can use
// the first 4 bytes of the header as forwarding address
// in garbage collection: If the (signed) Int32 is
// non-negative, the node has been forwarded already.

const False_tag = 0
const True_tag = 1
const Number_tag = 2
const Null_tag = 3
const Unassigned_tag = 4
const Undefined_tag = 5
const Blockframe_tag = 6
const Callframe_tag = 7
const Closure_tag = 8
const Frame_tag = 9 // 0000 1001
const Environment_tag = 10 // 0000 1010
const Pair_tag = 11
const Builtin_tag = 12
const String_tag = 13

let stringPool = {}
// all values (including literals) are allocated on the heap.

// We allocate canonical values for
// true, false, undefined, null, and unassigned
// and make sure no such values are created at runtime

// boolean values carry their value (0 for false, 1 for true)
// in the byte following the tag

let False
const is_False = (address) => heap_get_tag(address) === False_tag
let True
const is_True = (address) => heap_get_tag(address) === True_tag

const is_Boolean = (address) => is_True(address) || is_False(address)

let Null
const is_Null = (address) => heap_get_tag(address) === Null_tag

let Unassigned
const is_Unassigned = (address) => heap_get_tag(address) === Unassigned_tag

let Undefined
const is_Undefined = (address) => heap_get_tag(address) === Undefined_tag
let String
const is_String = (address) => heap_get_tag(address) === String_tag


const hashString = (str) => {
  let hash = 5381
  for (let i = 0; i < str.length; i++) {
    const char = str.charCodeAt(i)
    hash = (hash << 5) + hash + char
    hash = hash & hash
  }
  return hash >>> 0
}

const heap_allocate_String = (str) => {
  const hash = hashString(str)
  const address_or_undefined = stringPool[hash]

  if (address_or_undefined !== undefined) {
    return address_or_undefined[0]
  }

  const address = heap_allocate(String_tag, 1)
  heap_set_4_bytes_at_offset(address, 1, hash)

  // Store the string in the string pool
  stringPool[hash] = [address, str]

  return address
}

const heap_get_string_hash = (address) => heap_get_4_bytes_at_offset(address, 1)

const heap_get_string = (address) =>
  stringPool[heap_get_string_hash(address)][1]

const allocate_literal_values = () => {
  False = heap_allocate(False_tag, 1)
  True = heap_allocate(True_tag, 1)
  Null = heap_allocate(Null_tag, 1)
  Unassigned = heap_allocate(Unassigned_tag, 1)
  Undefined = heap_allocate(Undefined_tag, 1)
  String = heap_allocate(String_tag, 1)
}

// builtins: builtin id is encoded in second byte
// [1 byte tag, 1 byte id, 3 bytes unused,
//  2 bytes #children, 1 byte unused]
// Note: #children is 0

const is_Builtin = (address) => heap_get_tag(address) === Builtin_tag

const heap_allocate_Builtin = (id) => {
  const address = heap_allocate(Builtin_tag, 1)
  heap_set_byte_at_offset(address, 1, id)
  return address
}

const heap_get_Builtin_id = (address) => heap_get_byte_at_offset(address, 1)

// closure
// [1 byte tag, 1 byte arity, 2 bytes pc, 1 byte unused,
//  2 bytes #children, 1 byte unused]
// followed by the address of env
// note: currently bytes at offset 4 and 7 are not used;
//   they could be used to increase pc and #children range

const heap_allocate_Closure = (arity, pc, env) => {
  ALLOCATING = [env]
  const address = heap_allocate(Closure_tag, 2)
  ALLOCATING = []
  heap_set_byte_at_offset(address, 1, arity)
  heap_set_2_bytes_at_offset(address, 2, pc)
  heap_set(address + 1, env)
  return address
}

const heap_get_Closure_arity = (address) => heap_get_byte_at_offset(address, 1)

const heap_get_Closure_pc = (address) => heap_get_2_bytes_at_offset(address, 2)

const heap_get_Closure_environment = (address) => heap_get_child(address, 0)

const is_Closure = (address) => heap_get_tag(address) === Closure_tag

// block frame
// [1 byte tag, 4 bytes unused,
//  2 bytes #children, 1 byte unused]

const heap_allocate_Blockframe = (env) => {
  ALLOCATING = [env]
  const address = heap_allocate(Blockframe_tag, 2)
  heap_set(address + 1, env)
  ALLOCATING = []
  return address
}

const heap_get_Blockframe_environment = (address) => heap_get_child(address, 0)

const is_Blockframe = (address) => heap_get_tag(address) === Blockframe_tag

// call frame
// [1 byte tag, 1 byte unused, 2 bytes pc,
//  1 byte unused, 2 bytes #children, 1 byte unused]
// followed by the address of env

const heap_allocate_Callframe = (env, pc) => {
  ALLOCATING = [env]
  const address = heap_allocate(Callframe_tag, 2)
  ALLOCATING = []
  heap_set_2_bytes_at_offset(address, 2, pc)
  heap_set(address + 1, env)
  return address
}

const heap_get_Callframe_environment = (address) => heap_get_child(address, 0)

const heap_get_Callframe_pc = (address) =>
  heap_get_2_bytes_at_offset(address, 2)

const is_Callframe = (address) => heap_get_tag(address) === Callframe_tag

// environment frame
// [1 byte tag, 4 bytes unused,
//  2 bytes #children, 1 byte unused]
// followed by the addresses of its values

const heap_allocate_Frame = (number_of_values) =>
  heap_allocate(Frame_tag, number_of_values + 1)

const heap_Frame_display = (address) => {
  display('', 'Frame:')
  const size = heap_get_number_of_children(address)
  display(size, 'frame size:')
  for (let i = 0; i < size; i++) {
    display(i, 'value address:')
    const value = heap_get_child(address, i)
    display(value, 'value:')
    display(word_to_string(value), 'value word:')
  }
}

// environment
// [1 byte tag, 4 bytes unused,
//  2 bytes #children, 1 byte unused]
// followed by the addresses of its frames

const heap_allocate_Environment = (number_of_frames) =>
  heap_allocate(Environment_tag, number_of_frames + 1)

// access environment given by address
// using a "position", i.e. a pair of
// frame index and value index
const heap_get_Environment_value = (env_address, position) => {
  const [frame_index, value_index] = position
  const frame_address = heap_get_child(env_address, frame_index)
  return heap_get_child(frame_address, value_index)
}

const heap_set_Environment_value = (env_address, position, value) => {
  const [frame_index, value_index] = position
  const frame_address = heap_get_child(env_address, frame_index)
  heap_set_child(frame_address, value_index, value)
}

// extend a given environment by a new frame:
// create a new environment that is bigger by 1
// frame slot than the given environment.
// copy the frame Addresses of the given
// environment to the new environment.
// enter the address of the new frame to end
// of the new environment
const heap_Environment_extend = (frame_address, env_address) => {
  const old_size = heap_get_size(env_address)
  // modified: should not free frame address and env address here
  ALLOCATING = [frame_address, env_address]
  const new_env_address = heap_allocate_Environment(old_size)
  ALLOCATING = []
  let i
  for (i = 0; i < old_size - 1; i++) {
    heap_set_child(new_env_address, i, heap_get_child(env_address, i))
  }
  heap_set_child(new_env_address, i, frame_address)
  return new_env_address
}

// for debuggging: display environment
const heap_Environment_display = (env_address) => {
  const size = heap_get_number_of_children(env_address)
  display('', 'Environment:')
  display(size, 'environment size:')
  for (let i = 0; i < size; i++) {
    display(i, 'frame index:')
    const frame = heap_get_child(env_address, i)
    heap_Frame_display(frame)
  }
}

// pair
// [1 byte tag, 4 bytes unused,
//  2 bytes #children, 1 byte unused]
// followed by head and tail addresses, one word each
const heap_allocate_Pair = (hd, tl) => {
  const pair_address = heap_allocate(Pair_tag, 3)
  heap_set_child(pair_address, 0, hd)
  heap_set_child(pair_address, 1, tl)
  return pair_address
}

const is_Pair = (address) => heap_get_tag(address) === Pair_tag

// number
// [1 byte tag, 4 bytes unused,
//  2 bytes #children, 1 byte unused]
// followed by the number, one word
// note: #children is 0

const heap_allocate_Number = (n) => {
  const number_address = heap_allocate(Number_tag, 2)
  heap_set(number_address + 1, n)
  return number_address
}

const is_Number = (address) => heap_get_tag(address) === Number_tag

//
// conversions between addresses and JS_value
//

const address_to_JS_value = (x) =>
  is_Boolean(x)
    ? is_True(x)
      ? true
      : false
    : is_Number(x)
    ? heap_get(x + 1)
    : is_Undefined(x)
    ? undefined
    : is_Unassigned(x)
    ? '<unassigned>'
    : is_Null(x)
    ? null
    : is_String(x) // ADDED CHANGE
    ? heap_get_string(x) // ADDED CHANGE
    : is_Pair(x)
    ? [
        address_to_JS_value(heap_get_child(x, 0)),
        address_to_JS_value(heap_get_child(x, 1)),
      ]
    : is_Closure(x)
    ? '<closure>'
    : is_Builtin(x)
    ? '<builtin>'
    : 'unknown word tag: ' + word_to_string(x)

const JS_value_to_address = (x) =>
  is_boolean(x)
    ? x
      ? True
      : False
    : is_number(x)
    ? heap_allocate_Number(x)
    : is_undefined(x)
    ? Undefined
    : is_null(x)
    ? Null
    : is_string(x) // ADDED CHANGE
    ? heap_allocate_String(x) // ADDED CHANGE
    : is_pair(x)
    ? heap_allocate_Pair(
        JS_value_to_address(head(x)),
        JS_value_to_address(tail(x))
      )
    : 'unknown word tag: ' + word_to_string(x)

// ************************
// compile-time environment
// ************************/

// a compile-time environment is an array of
// compile-time frames, and a compile-time frame
// is an array of symbols

// find the position [frame-index, value-index]
// of a given symbol x
const compile_time_environment_position = (env, x) => {
  let frame_index = env.length
  while (value_index(env[--frame_index], x) === -1) {}
  return [frame_index, value_index(env[frame_index], x)]
}

const value_index = (frame, x) => {
  for (let i = 0; i < frame.length; i++) {
    if (frame[i] === x) return i
  }
  return -1
}

// in this machine, the builtins take their
// arguments directly from the operand stack,
// to save the creation of an intermediate
// argument array
const builtin_implementation = {
  display: () => {
    const address = OS.pop()
    display(address_to_JS_value(address))
    return address
  },
  error: () => error(address_to_JS_value(OS.pop())),
  is_number: () => (is_Number(OS.pop()) ? True : False),
  is_boolean: () => (is_Boolean(OS.pop()) ? True : False),
  is_undefined: () => (is_Undefined(OS.pop()) ? True : False),
  is_string: () => (is_String(OS.pop()) ? True : False), // ADDED CHANGE
  is_function: () => is_Closure(OS.pop()),
  pair: () => {
    const tl = OS.pop()
    const hd = OS.pop()
    return heap_allocate_Pair(hd, tl)
  },
  is_pair: () => (is_Pair(OS.pop()) ? True : False),
  head: () => heap_get_child(OS.pop(), 0),
  tail: () => heap_get_child(OS.pop(), 1),
  is_null: () => (is_Null(OS.pop()) ? True : False),
  set_head: () => {
    const val = OS.pop()
    const p = OS.pop()
    heap_set_child(p, 0, val)
  },
  set_tail: () => {
    const val = OS.pop()
    const p = OS.pop()
    heap_set_child(p, 1, val)
  },
}

const builtins = {}
const builtin_array = []
{
  let i = 0
  for (const key in builtin_implementation) {
    builtins[key] = {
      tag: 'BUILTIN',
      id: i,
      arity: arity(builtin_implementation[key]),
    }
    builtin_array[i++] = builtin_implementation[key]
  }
}

const constants = {
  undefined: Undefined,
  math_E: math_E,
  math_LN10: math_LN10,
  math_LN2: math_LN2,
  math_LOG10E: math_LOG10E,
  math_LOG2E: math_LOG2E,
  math_PI: math_PI,
  math_SQRT1_2: math_SQRT1_2,
  math_SQRT2: math_SQRT2,
}

const compile_time_environment_extend = (vs, e) => {
  //  make shallow copy of e
  return push([...e], vs)
}

// compile-time frames only need synbols (keys), no values
const builtin_compile_frame = Object.keys(builtins)
const constant_compile_frame = Object.keys(constants)
const global_compile_environment = [
  builtin_compile_frame,
  constant_compile_frame,
]

/* ********
 * compiler
 * ********/

// scanning out the declarations from (possibly nested)
// sequences of statements, ignoring blocks
const scan = (comp, ce) => {
  let result
  if (comp.tag === 'seq') {
    result = comp.stmts.reduce((acc, x) => acc.concat(scan(x, ce)), [])
  } else if (['assmt'].includes(comp.tag)) {
    let temp = []
    for (let i = 0; i < comp.sym.length; i++) {
      let flag = false
      for (let j = 0; j < ce.length; j++) {
        if (ce[j].includes(comp.sym[i].sym)) {
          flag = true
        }
      }
      if (flag != true) {
        temp.push(comp.sym[i].sym)
      }
    }
    result = temp
  } else if (['decl'].includes(comp.tag)) {
    let temp = []
    for (let i = 0; i < comp.sym.length; i++) {
      temp.push(comp.sym[i].sym)
    }
    result = temp
  } else if (['fun', 'waitgroupdecl'].includes(comp.tag)) {
    result = [comp.sym]
  } else {
    result = []
  }
  return result
}

const compile_sequence = (seq, ce) => {
  if (seq.length === 0) return (instrs[wc++] = { tag: 'LDC', val: undefined })
  let first = true
  for (let comp of seq) {
    first ? (first = false) : (instrs[wc++] = { tag: 'POP' })
    compile(comp, ce)
  }
}

// wc: write counter
let wc
// instrs: instruction array
let instrs

const compile_comp = {
  lit: (comp, ce) => {
    instrs[wc++] = { tag: 'LDC', val: comp.val }
  },
  nam:
    // store precomputed position information in LD instruction
    (comp, ce) => {
      instrs[wc++] = {
        tag: 'LD',
        sym: comp.sym,
        pos: compile_time_environment_position(ce, comp.sym),
      }
    },
  unop: (comp, ce) => {
    compile(comp.frst, ce)
    instrs[wc++] = { tag: 'UNOP', sym: comp.sym }
  },
  binop: (comp, ce) => {
    compile(comp.frst, ce)
    compile(comp.scnd, ce)
    instrs[wc++] = { tag: 'BINOP', sym: comp.sym }
  },
  log: (comp, ce) => {
    compile(
      comp.sym == '&&'
        ? {
            tag: 'cond_expr',
            pred: comp.frst,
            cons: { tag: 'lit', val: true },
            alt: comp.scnd,
          }
        : {
            tag: 'cond_expr',
            pred: cmd.frst,
            cons: cmd.scnd,
            alt: { tag: 'lit', val: false },
          },
      ce
    )
  },
  cond: (comp, ce) => {
    if (comp.alt !== undefined) {
      compile(comp.pred, ce)
      const jump_on_false_instruction = { tag: 'JOF' }
      instrs[wc++] = jump_on_false_instruction
      compile(comp.cons, ce)
      const goto_instruction = { tag: 'GOTO' }
      instrs[wc++] = goto_instruction
      const alternative_address = wc
      jump_on_false_instruction.addr = alternative_address
      compile(comp.alt, ce)
      goto_instruction.addr = wc
    } else {
      compile(comp.pred, ce)
      // Jump on false needs to skip over the 'then' block if the condition is false
      const jump_on_false_instruction = { tag: 'JOF' }
      instrs[wc++] = jump_on_false_instruction
      compile(comp.cons, ce)
      // Set the address to jump to if the condition is false
      jump_on_false_instruction.addr = wc
    }
  },
  for: (comp, ce) => {
    const loop_start = wc
    compile(comp.pred, ce)
    const jump_on_false_instruction = { tag: 'JOF' }
    instrs[wc++] = jump_on_false_instruction
    compile(comp.body, ce)
    instrs[wc++] = { tag: 'POP' }
    instrs[wc++] = { tag: 'GOTO', addr: loop_start }
    jump_on_false_instruction.addr = wc
    instrs[wc++] = { tag: 'LDC', val: undefined }
  },
  assmt:
    // store precomputed position info in ASSIGN instruction
    (comp, ce) => {
      for (let i = 0; i < comp.expr.length; i++) {
        compile(comp.expr[i], ce)
        instrs[wc++] = {
          tag: 'ASSIGN',
          pos: compile_time_environment_position(ce, comp.sym[i].sym),
        }
      }
    },
  decl:
    // store precomputed position info in ASSIGN instruction
    (comp, ce) => {
      for (let i = 0; i < comp.expr.length; i++) {
        compile(comp.expr[i], ce)
        instrs[wc++] = {
          tag: 'ASSIGN',
          pos: compile_time_environment_position(ce, comp.sym[i].sym),
        }
      }
    },
  fundecl:
    // store precomputed position info in ASSIGN instruction
    (comp, ce) => {
      compile(comp.expr, ce)
      instrs[wc++] = {
        tag: 'ASSIGN',
        pos: compile_time_environment_position(ce, comp.sym),
      }
    },
  lam: (comp, ce) => {
    instrs[wc++] = { tag: 'LDF', arity: comp.arity, addr: wc + 1 }
    // jump over the body of the lambda expression
    const goto_instruction = { tag: 'GOTO' }
    instrs[wc++] = goto_instruction
    // extend compile-time environment
    compile(comp.body, compile_time_environment_extend(comp.prms, ce))
    instrs[wc++] = { tag: 'LDC', val: undefined }
    instrs[wc++] = { tag: 'RESET' }
    goto_instruction.addr = wc
  },
  ret: (comp, ce) => {
    compile(comp.expr, ce)
    if (comp.expr.tag === 'app') {
      // tail call: turn CALL into TAILCALL
      instrs[wc - 1].tag = 'TAIL_CALL'
    } else {
      instrs[wc++] = { tag: 'RESET' }
    }
  },
  fun: (comp, ce) => {
    compile(
      {
        tag: 'fundecl',
        sym: comp.sym,
        expr: { tag: 'lam', prms: comp.prms, body: comp.body },
      },
      ce
    )
  },
  app: (comp, ce) => {
    compile(comp.fun, ce)
    for (let arg of comp.args) {
      compile(arg, ce)
    }
    instrs[wc++] = { tag: 'CALL', arity: comp.args.length }
  },
  seq: (comp, ce) => compile_sequence(comp.stmts, ce),
  blk: (comp, ce) => {
    const locals = scan(comp.body, ce)
    if (locals.length !== 0) {
      instrs[wc++] = { tag: 'ENTER_SCOPE', num: locals.length }
      compile(comp.body, compile_time_environment_extend(locals, ce))
      instrs[wc++] = { tag: 'EXIT_SCOPE' }
      return
    }
    if (locals.length == 0 && ce.length == 1) {
      instrs[wc++] = { tag: 'ENTER_SCOPE', num: locals.length }
      compile(comp.body, compile_time_environment_extend(locals, ce))
      instrs[wc++] = { tag: 'EXIT_SCOPE' }
      return
    }
    compile(comp.body, compile_time_environment_extend(locals, ce))
  },
  waitgroupdecl: (comp, ce) => {
    instrs[wc++] = {
      tag: 'WAITGROUPDECL',
      sym: comp.sym,
      pos: compile_time_environment_position(ce, comp.sym),
    }
  },
  deferStmt: (comp, ce) => {
    //set automatically decrement the counter once a go routine is ended
  },
  addwait: (comp, ce) => {
    compile(comp.val, ce)
    instrs[wc++] = {
      tag: 'WAITGROUPADD',
      sym: comp.sym.sym,
      pos: compile_time_environment_position(ce, comp.sym.sym),
    }
  },
  wait: (comp, ce) => {
    instrs[wc++] = {
      tag: 'WAIT',
      sym: comp.sym.sym,
      pos: compile_time_environment_position(ce, comp.sym.sym),
    }
  },
  makechannel: (comp, ce) => {
    instrs[wc++] = { tag: 'MAKECHANNEL' }
  },
  send: (comp, ce) => {
    compile(comp.val, ce)
    let pos = []
    for (let i = 1; i < ce.length; i++) {
      for (let j = 0; j < ce[i].length; j++) {
        if (ce[i][j] == comp.chan) {
          pos.push([i, j])
        }
      }
    }
    instrs[wc++] = {
      tag: 'SEND',
      chan: comp.chan,
      pos: pos.shift(),
    }
  },
  receive: (comp, ce) => {
    let pos = []
    for (let i = 1; i < ce.length; i++) {
      for (let j = 0; j < ce[i].length; j++) {
        if (ce[i][j] == comp.chan) {
          pos.push([i, j])
        }
      }
    }
    instrs[wc++] = {
      tag: 'RECEIVE',
      chan: comp.chan,
      pos: pos.shift(),
    }
  },
  gostmt: (comp, ce) => {
    // Compile the body of the goroutine
    // compile(comp.callbody, ce)
    // Create a new closure for the goroutine
    instrs[wc++] = { tag: 'LDF', arity: 0, addr: wc + 1 }
    // Jump over the body of the closure
    const goto_instruction = { tag: 'GOTO' }
    instrs[wc++] = goto_instruction
    // Compile the body of the closure
    compile(comp.callbody, ce)
    instrs[wc++] = { tag: 'LDC', val: undefined }
    instrs[wc++] = { tag: 'ENDTHREAD' }
    goto_instruction.addr = wc
    instrs[wc++] = { tag: 'STARTTHREAD' }
  },
  //display for fmt.println
  display: (comp, ce) => {
    for (let i = 0; i < comp.content.length; i++) {
      // display("++++++++++++++++++")
      // display(comp.content[i])
      compile(comp.content[i], ce)
    }
    instrs[wc++] = { tag: 'DISPLAY', num: comp.content.length }
  },
}

// compile component into instruction array instrs,
// starting at wc (write counter)
const compile = (comp, ce) => {
  compile_comp[comp.tag](comp, ce)
}

// compile program into instruction array instrs,
// after initializing wc and instrs
const compile_program = (program) => {
  wc = 0
  instrs = []
  compile(program, global_compile_environment)
  instrs[wc] = { tag: 'DONE' }
  // print_code()
}
const print_code = () => {
  for (let i = 0; i < instrs.length; i = i + 1) {
    const instr = instrs[i]
    // display(i)
    // display(instr)
    display(
      '',
      stringify(i) +
        ': ' +
        instr.tag +
        ' ' +
        (instr.tag === 'GOTO' ? stringify(instr.addr) : '') +
        (instr.tag === 'ENTER_SCOPE' ? stringify(instr.num) : '') +
        (instr.tag === 'LDC' ? stringify(instr.val) : '') +
        (instr.tag === 'ASSIGN' ? stringify(instr.pos) : '') +
        (instr.tag === 'LD' ? stringify(instr.pos) : '') +
        (instr.tag === 'BINOP' ? stringify(instr.sym) : '') +
        (instr.tag === 'JOF' ? stringify(instr.addr) : '')
    )
  }
}
// **********************
// operators and builtins
// **********************/

const binop_microcode = {
  '+': (x, y) =>
    (is_number(x) && is_number(y)) || (is_string(x) && is_string(y))
      ? x + y
      : error([x, y], '+ expects two numbers' + ' or two strings, got:'),
  // todo: add error handling to JS for the following, too
  '*': (x, y) => x * y,
  '-': (x, y) => x - y,
  '/': (x, y) => x / y,
  '%': (x, y) => x % y,
  '<': (x, y) => x < y,
  '<=': (x, y) => x <= y,
  '>=': (x, y) => x >= y,
  '>': (x, y) => x > y,
  '===': (x, y) => x === y,
  '==': (x, y) => x == y,
  '!==': (x, y) => x !== y,
}

// v2 is popped before v1
const apply_binop = (op, v2, v1) =>
  JS_value_to_address(
    binop_microcode[op](address_to_JS_value(v1), address_to_JS_value(v2))
  )

const unop_microcode = {
  '-unary': (x) => -x,
  '!': (x) => !x,
}

const apply_unop = (op, v) =>
  JS_value_to_address(unop_microcode[op](address_to_JS_value(v)))

const apply_builtin = (builtin_id) => {
  display(builtin_id, 'apply_builtin: builtin_id:')
  const result = builtin_array[builtin_id]()
  OS.pop() // pop fun
  push(OS, result)
}

const allocate_builtin_frame = () => {
  const builtin_values = Object.values(builtins)
  const frame_address = heap_allocate_Frame(builtin_values.length)
  for (let i = 0; i < builtin_values.length; i++) {
    const builtin = builtin_values[i]
    heap_set_child(frame_address, i, heap_allocate_Builtin(builtin.id))
  }
  return frame_address
}

const allocate_constant_frame = () => {
  const constant_values = Object.values(constants)
  const frame_address = heap_allocate_Frame(constant_values.length)
  for (let i = 0; i < constant_values.length; i++) {
    const constant_value = constant_values[i]
    if (typeof constant_value === 'undefined') {
      heap_set_child(frame_address, i, Undefined)
    } else {
      heap_set_child(frame_address, i, heap_allocate_Number(constant_value))
    }
  }
  return frame_address
}

/* *******
 * machine
 * *******/

// machine registers
let OS // JS array (stack) of words (Addresses,
//        word-encoded literals, numbers)
let PC // JS number
let E // heap Address
let RTS // JS array (stack) of Addresses
HEAP // (declared above already)

const microcode = {
  LDC: (instr) => push(OS, JS_value_to_address(instr.val)),
  UNOP: (instr) => push(OS, apply_unop(instr.sym, OS.pop())),
  BINOP: (instr) => push(OS, apply_binop(instr.sym, OS.pop(), OS.pop())),
  POP: (instr) => OS.pop(),
  JOF: (instr) => (PC = is_True(OS.pop()) ? PC : instr.addr),
  GOTO: (instr) => (PC = instr.addr),
  ENTER_SCOPE: (instr) => {
    push(RTS, heap_allocate_Blockframe(E))
    const frame_address = heap_allocate_Frame(instr.num)
    E = heap_Environment_extend(frame_address, E)
    for (let i = 0; i < instr.num; i++) {
      heap_set_child(frame_address, i, Unassigned)
    }
  },
  EXIT_SCOPE: (instr) => (E = heap_get_Blockframe_environment(RTS.pop())),
  LD: (instr) => {
    const val = heap_get_Environment_value(E, instr.pos)
    if (is_Unassigned(val)) error('access of unassigned variable')
    push(OS, val)
  },
  ASSIGN: (instr) => heap_set_Environment_value(E, instr.pos, peek(OS, 0)),
  LDF: (instr) => {
    const closure_address = heap_allocate_Closure(instr.arity, instr.addr, E)
    push(OS, closure_address)
  },
  CALL: (instr) => {
    const arity = instr.arity
    const fun = peek(OS, arity)
    if (is_Builtin(fun)) {
      return apply_builtin(heap_get_Builtin_id(fun))
    }
    const frame_address = heap_allocate_Frame(arity)
    for (let i = arity - 1; i >= 0; i--) {
      heap_set_child(frame_address, i, OS.pop())
    }
    OS.pop() // pop fun
    push(RTS, heap_allocate_Callframe(E, PC))
    E = heap_Environment_extend(
      frame_address,
      heap_get_Closure_environment(fun)
    )
    PC = heap_get_Closure_pc(fun)
  },
  TAIL_CALL: (instr) => {
    const arity = instr.arity
    const fun = peek(OS, arity)
    if (is_Builtin(fun)) {
      return apply_builtin(heap_get_Builtin_id(fun))
    }
    const frame_address = heap_allocate_Frame(arity)
    for (let i = arity - 1; i >= 0; i--) {
      heap_set_child(frame_address, i, OS.pop())
    }
    OS.pop() // pop fun
    // don't push on RTS here
    E = heap_Environment_extend(
      frame_address,
      heap_get_Closure_environment(fun)
    )
    PC = heap_get_Closure_pc(fun)
  },
  RESET: (instr) => {
    PC--
    // keep popping...
    const top_frame = RTS.pop()
    if (is_Callframe(top_frame)) {
      // ...until top frame is a call frame
      PC = heap_get_Callframe_pc(top_frame)
      E = heap_get_Callframe_environment(top_frame)
    }
  },
  DISPLAY: (instr) => {
    const num = instr.num
    const values = []
    for (let i = 0; i < num; i++) {
      values.push(OS.pop())
    }
    // display(values.map(address_to_JS_value).reverse())
    result.push(values.map(address_to_JS_value).reverse())
  },
  STARTTHREAD: (instr) => {
    const closure = OS.pop()
    const newOS = []
    const newE = E
    const newRTS = []
    const newPC = heap_get_Closure_pc(closure)
    const newThread = new Thread(newOS, newE, newRTS, newPC)
    newThread.isRunning = true
    if (WAIT_GROUP_POS.length != 0) {
      newThread.waitgroup_pos = WAIT_GROUP_POS.pop()
    }
    threadQueue.push(newThread)
  },
  WAITGROUPDECL: (instr) => {
    update_global_E(instr.pos, JS_value_to_address(0))
  },
  WAITGROUPADD: (instr) => {
    //开一个新的位置等人拿
    const add_delta = OS.pop()
    for (let i = 0; i < address_to_JS_value(add_delta); i++) {
      WAIT_GROUP_POS.push(instr.pos)
    }
    const original_counter = heap_get_Environment_value(E, instr.pos)
    const new_counter = apply_binop('+', add_delta, original_counter)
    update_global_E(instr.pos, new_counter)
  },
  MAKECHANNEL: (instr) => {
    push(OS, JS_value_to_address(undefined))
  },

  SEND: (instr) => {
    //If enter by coincidence, return to last PC which next PC is SEND
    if (!SenderReady || !ReceiverReady) {
      PC = PC - 2
      return
    }
    update_global_E(instr.pos, instr.val)
  },

  RECEIVE: (instr) => {
    //If enter by coincidence, return to last PC which next PC is Receive
    if ((!SenderReady)||(!ReceiverReady)){
     PC=PC-2
     return
    }
    const val = heap_get_Environment_value(E, instr.pos)
    push(OS, val)
    ReceiverReady=false
    SenderReady=false
  },
}

// running the machine

// set up registers, including free list
function initialize_machine(heapsize_words) {
  stringPool = {}
  OS = []
  PC = 0
  RTS = []
  ALLOCATING = []
  HEAP_BOTTOM = undefined

  HEAP = heap_make(heapsize_words)
  heap_size = heapsize_words
  let i = 0
  for (i = 0; i <= heapsize_words - node_size; i = i + node_size) {
    heap_set(i, i + node_size)
  }
  heap_set(i - node_size, -1)
  free = 0
  PC = 0
  allocate_literal_values()
  const builtins_frame = allocate_builtin_frame()
  const constants_frame = allocate_constant_frame()
  E = heap_allocate_Environment(0)
  E = heap_Environment_extend(builtins_frame, E)
  E = heap_Environment_extend(constants_frame, E)
  HEAP_BOTTOM = free
}
class Thread {
  constructor(os, e, rts, pc) {
    this.os = os
    this.e = e
    this.rts = rts
    this.pc = pc
    this.isRunning = false
    this.waitgroup_pos = null
  }

  saveContext() {
    this.os = [...OS]
    this.e = E
    this.rts = [...RTS]
    this.pc = PC
  }
}
function update_global_E(pos, value) {
  for (let i = 0; i < threadQueue.length; i++) {
    const env = threadQueue[i].e
    heap_set_Environment_value(env, pos, value)
  }
  heap_set_Environment_value(E, pos, value)
}

let SenderReady=false
let ReceiverReady=false
let WAIT_GROUP_POS = []
const waited_thread_queue = []
let threadQueue = []
const timeSlice = 1

async function run() {
  initialize_machine(100000000)
  result = []
  // Create the main goroutine
  const mainThread = new Thread([], E, [], 0)
  mainThread.isRunning = true
  threadQueue.push(mainThread)

  while (threadQueue.length > 0) {
    let instructionCount = 0
    let currentThread
    let from_waited_queue = false
    if (waited_thread_queue.length > 0) {
      // Prioritize threads in the waited_thread_queue
      currentThread = waited_thread_queue.shift()
      from_waited_queue = true
    } else {
      currentThread = threadQueue.shift()
    }
    //把排到的thread拿出来
    //OS,PC,E,RTS 总是暂存正在用的thread的环境
    OS = currentThread.os
    PC = currentThread.pc
    E = currentThread.e
    RTS = currentThread.rts

    while (
      instructionCount < timeSlice &&
      currentThread.isRunning &&
      !(instrs[PC].tag === 'DONE' || instrs[PC].tag === 'ENDTHREAD') &&
      !(instrs[PC].tag === 'WAIT')
    ) {
      //跑mainthread的function
      const instr = instrs[PC++]
      await microcode[instr.tag](instr)
      instructionCount++
      //next instruction is receive
      if (instrs[PC].tag === 'RECEIVE') {
        //if it is the only thread currently and receiving is before sending, stop running and give error
        if ((waited_thread_queue.length==0)&&(threadQueue.length==0)&&(!SenderReady)){
         result.push(['fatal error: all goroutines are asleep - deadlock!'])
         currentThread.isRunning = false
         threadQueue.splice(0, threadQueue.length)
         break
        }
        ReceiverReady=true
        if (!SenderReady){
         PC--
         break
        }
        const current_value_in_channel = address_to_JS_value(
          heap_get_Environment_value(E, instrs[PC].pos)
        )
        if (current_value_in_channel == undefined) {
          PC--
          break
        }
      }
      //next instruction is send
      // will not execute until next receive exist
      if (instrs[PC].tag === 'SEND') {
       SenderReady = true
       const value = OS.pop()
       instrs[PC].val = value
       //Currently main go routine is the only routine and sending is before receiving, stop running and give error
       if ((waited_thread_queue.length==0)&&(threadQueue.length==0)&&(!ReceiverReady)){
         result.push(['fatal error: all goroutines are asleep - deadlock!'])
         currentThread.isRunning = false
         threadQueue.splice(0, threadQueue.length)
         break
        }
       if (!ReceiverReady){
        PC--
        break
       }
      }
    }
    //check next instruction which is not actually run in microcode since they have to modify the current thread
    if (instrs[PC].tag === 'ENDTHREAD') {
      currentThread.isRunning = false
      //decrease the wait group counter globally
      const original_counter = heap_get_Environment_value(
        E,
        currentThread.waitgroup_pos
      )
      const new_counter = apply_binop(
        '-',
        original_counter,
        JS_value_to_address(1)
      )
      update_global_E(currentThread.waitgroup_pos, new_counter)

      currentThread.waitgroup_pos = null
      threadQueue = threadQueue.filter((thread) => thread.isRunning)
    }
    if (instrs[PC].tag === 'DONE') {
      currentThread.isRunning = false
      threadQueue.splice(0, threadQueue.length)
    }
    if (instrs[PC].tag === 'WAIT') {
      const counter_value = address_to_JS_value(
        heap_get_Environment_value(E, instrs[PC].pos)
      )
      if (
        JSON.stringify(currentThread.waitgroup_pos) ==
        JSON.stringify(instrs[PC].pos)
      ) {
        waited_thread_queue.push(currentThread)
      }
      if (counter_value != 0) {
        for (let i = 0; i < threadQueue.length; i++) {
          if (
            JSON.stringify(threadQueue[i].waitgroup_pos) ==
            JSON.stringify(instrs[PC].pos)
          ) {
            waited_thread_queue.push(threadQueue[i])
          }
        }
      }

      PC++
    }
    if (currentThread.isRunning) {
      currentThread.saveContext()
      if (from_waited_queue == true) {
        waited_thread_queue.push(currentThread)
      } else {
        threadQueue.push(currentThread)
      }
    }
  }
}

let result = []
const run_vm = async (jsonAST) => {
  return new Promise((resolve, reject) => {
    try {
      const json = JSON.parse(jsonAST)
      compile_program(json)
      run()
        .then(() => {
          resolve({ result: result })
        })
        .catch((error) => {
          reject(error)
        })
    } catch (error) {
      reject(error)
    }
  })
}
module.exports = run_vm
