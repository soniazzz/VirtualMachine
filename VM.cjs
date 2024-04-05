const { display } = require('sicp')

Object.entries(require('sicp')).forEach(
  ([name, exported]) => (global[name] = exported)
)

const push = (array, ...items) => {
  for (let item of items) {
    array.push(item)
  }
  return array
}

// return the last element of given array
// without changing the array
const peek = (array, address) => array.slice(-1 - address)[0]

/* *************************
 * HEAP
 * *************************/

// HEAP is an array of bytes (JS ArrayBuffer)

const word_size = 8
const mega = 2 ** 20

// heap_make allocates a heap of given size
// (in megabytes)and returns a DataView of that,
// see https://www.javascripture.com/DataView
const heap_make = (bytes) => {
  if (bytes % 8 !== 0) error('heap bytes must be divisible by 8')
  const data = new ArrayBuffer(bytes)
  const view = new DataView(data)
  return view
}

// we randomly pick a heap size of 1000000 bytes
const HEAP = heap_make(1000000)

// free is the next free index in HEAP
// we keep allocating as if there was no tomorrow
let free = 0

// for debugging: display all bits of the heap
const heap_display = () => {
  display('', 'heap:')
  for (let i = 0; i < free; i++) {
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
const heap_allocate = (tag, size) => {
  const address = free
  free += size
  HEAP.setUint8(address * word_size, tag)
  HEAP.setUint16(address * word_size + size_offset, size)
  return address
}

// get and set a word in heap at given address
const heap_get = (address) => HEAP.getFloat64(address * word_size)

const heap_set = (address, x) => HEAP.setFloat64(address * word_size, x)

// child index starts at 0
const heap_get_child = (address, child_index) =>
  heap_get(address + 1 + child_index)

const heap_set_child = (address, child_index, value) =>
  heap_set(address + 1 + child_index, value)

const heap_get_tag = (address) => HEAP.getUint8(address * word_size)

const heap_get_size = (address) =>
  HEAP.getUint16(address * word_size + size_offset)

// the number of children is one less than the size
// except for number nodes:
//                 they have size 2 but no children
const heap_get_number_of_children = (address) =>
  heap_get_tag(address) === Number_tag ? 0 : get_size(address) - 1

// access byte in heap, using address and offset
const heap_set_byte_at_offset = (address, offset, value) =>
  HEAP.setUint8(address * word_size + offset, value)

const heap_get_byte_at_offset = (address, offset, value) =>
  HEAP.getUint8(address * word_size + offset)

// access byte in heap, using address and offset
const heap_set_2_bytes_at_offset = (address, offset, value) =>
  HEAP.setUint16(address * word_size + offset, value)

const heap_get_2_bytes_at_offset = (address, offset, value) =>
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

const False_tag = 0
const True_tag = 1
const Number_tag = 2
const Null_tag = 3
const Unassigned_tag = 4
const Undefined_tag = 5
const Blockframe_tag = 6
const Callframe_tag = 7
const Closure_tag = 8
const Frame_tag = 9
const Environment_tag = 10
const Pair_tag = 11
const Builtin_tag = 12
const String_tag = 13 // ADDED CHANGE

// Record<string, tuple(number, string)> where the key is the hash of the string
// and the value is a tuple of the address of the string and the string itself
let stringPool = {} // ADDED CHANGE

// all values (including literals) are allocated on the heap.

// We allocate canonical values for
// true, false, undefined, null, and unassigned
// and make sure no such values are created at runtime

// boolean values carry their value (0 for false, 1 for true)
// in the byte following the tag
const False = heap_allocate(False_tag, 1)
const is_False = (address) => heap_get_tag(address) === False_tag
const True = heap_allocate(True_tag, 1)
const is_True = (address) => heap_get_tag(address) === True_tag

const is_Boolean = (address) => is_True(address) || is_False(address)

const Null = heap_allocate(Null_tag, 1)
const is_Null = (address) => heap_get_tag(address) === Null_tag

const Unassigned = heap_allocate(Unassigned_tag, 1)
const is_Unassigned = (address) => heap_get_tag(address) === Unassigned_tag

const Undefined = heap_allocate(Undefined_tag, 1)
const is_Undefined = (address) => heap_get_tag(address) === Undefined_tag

// ADDED CHANGE
// strings:
// [1 byte tag, 4 byte hash to stringPool,
// 2 bytes #children, 1 byte unused]
// Note: #children is 0

// Hash any string to a 32-bit unsigned integer
const hashString = (str) => {
  let hash = 5381
  for (let i = 0; i < str.length; i++) {
    const char = str.charCodeAt(i)
    hash = (hash << 5) + hash + char
    hash = hash & hash
  }
  return hash >>> 0
}

// const result = hashString("hello");
// display(result, "hash of hello:");
// const result2 = hashString("hello world");
// display(result2, "hash of hello world:");

const String = heap_allocate(String_tag, 1)
const is_String = (address) => heap_get_tag(address) === String_tag

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
  const address = heap_allocate(Closure_tag, 2)
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
  const address = heap_allocate(Blockframe_tag, 2)
  heap_set(address + 1, env)
  return address
}

const heap_get_Blockframe_environment = (address) => heap_get_child(address, 0)

const is_Blockframe = (address) => heap_get_tag(address) === Blockframe_tag

// call frame
// [1 byte tag, 1 byte unused, 2 bytes pc,
//  1 byte unused, 2 bytes #children, 1 byte unused]
// followed by the address of env

const heap_allocate_Callframe = (env, pc) => {
  const address = heap_allocate(Callframe_tag, 2)
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

const heap_empty_Environment = heap_allocate_Environment(0)

// access environment given by address
// using a "position", i.e. a pair of
// frame index and value index
const heap_get_Environment_value = (env_address, position) => {
  const [frame_index, value_index] = position
  const frame_address = heap_get_child(env_address, frame_index)
  return heap_get_child(frame_address, value_index)
}

const heap_set_Environment_value = (env_address, position, value) => {
  //display(env_address, "env_address:")
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
  const new_env_address = heap_allocate_Environment(old_size)
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

/* ************************
 * compile-time environment
 * ************************/

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
const builtin_object = {
  display: () => {
    const address = OS.pop()
    display(address_to_JS_value(address))
    return address
  },
  get_time: () => JS_value_to_address(get_time()),
  error: () => error(address_to_JS_value(OS.pop())),
  is_number: () => (is_Number(OS.pop()) ? True : False),
  is_boolean: () => (is_Boolean(OS.pop()) ? True : False),
  is_undefined: () => (is_Undefined(OS.pop()) ? True : False),
  is_string: () => (is_String(OS.pop()) ? True : False), // ADDED CHANGE
  is_function: () => is_Closure(OS.pop()),
  math_sqrt: () =>
    JS_value_to_address(math_sqrt(address_to_JS_value(OS.pop()))),
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

const primitive_object = {}
const builtin_array = []
{
  let i = 0
  for (const key in builtin_object) {
    primitive_object[key] = {
      tag: 'BUILTIN',
      id: i,
      arity: arity(builtin_object[key]),
    }
    builtin_array[i++] = builtin_object[key]
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

for (const key in constants) primitive_object[key] = constants[key]

const compile_time_environment_extend = (vs, e) => {
  //  make shallow copy of e
  return push([...e], vs)
}

// compile-time frames only need synbols (keys), no values
const global_compile_frame = Object.keys(primitive_object)
const global_compile_environment = [global_compile_frame]

/* ********
 * compiler
 * ********/

// scanning out the declarations from (possibly nested)
// sequences of statements, ignoring blocks
const scan = (comp, ce) => {
  let result
  if (comp.tag === 'seq') {
    result = comp.stmts.reduce((acc, x) => acc.concat(scan(x, ce)), [])
  } else if (['assmt', 'fun'].includes(comp.tag)) {
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
  print_code()
}

/* **********************
 * operators and builtins
 * **********************/

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
const apply_binop = (op, v2, v1) => {
  let result = JS_value_to_address(
    binop_microcode[op](address_to_JS_value(v1), address_to_JS_value(v2))
  )
  return result
}

const unop_microcode = {
  '-unary': (x) => -x,
  '!': (x) => !x,
}

const apply_unop = (op, v) =>
  JS_value_to_address(unop_microcode[op](address_to_JS_value(v)))

const apply_builtin = (builtin_id) => {
  const result = builtin_array[builtin_id]()
  OS.pop() // pop fun
  push(OS, result)
}

// creating global runtime environment
const primitive_values = Object.values(primitive_object)
const frame_address = heap_allocate_Frame(primitive_values.length)
for (let i = 0; i < primitive_values.length; i++) {
  const primitive_value = primitive_values[i]
  if (
    typeof primitive_value === 'object' &&
    primitive_value.hasOwnProperty('id')
  ) {
    heap_set_child(frame_address, i, heap_allocate_Builtin(primitive_value.id))
  } else if (typeof primitive_value === 'undefined') {
    heap_set_child(frame_address, i, Undefined)
  } else {
    heap_set_child(frame_address, i, heap_allocate_Number(primitive_value))
  }
}

const global_environment = heap_Environment_extend(
  frame_address,
  heap_empty_Environment
)

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
  // JOF: (instr) => {
  //   display("haha")
  //   print_OS()
  //   let result=is_True(OS.pop())
  //   display(result)
  //   PC = result ? PC : instr.addr},
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
}

function run() {
  OS = []
  PC = 0
  E = global_environment
  RTS = []
  stringPool = {} // ADDED CHANGE
  //print_code()
  while (!(instrs[PC].tag === 'DONE')) {
    //heap_display()
    //display(PC, "PC: ")
    // display(instrs[PC].tag, 'instr: ')
    //print_OS("\noperands:            ");
    //print_RTS("\nRTS:            ");
    const instr = instrs[PC++]
    // display(instrs[PC].tag, "next instruction: ")
    microcode[instr.tag](instr)
  }
  // display(OS, '\nfinal operands:           ')
  // print_OS()
  return address_to_JS_value(peek(OS, 0))
}

// debugging

const print_code = () => {
  for (let i = 0; i < instrs.length; i = i + 1) {
    const instr = instrs[i]
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

const print_RTS = (x) => {
  display('', x)
  for (let i = 0; i < RTS.length; i = i + 1) {
    const f = RTS[i]
    display('', stringify(i) + ': ' + f.tag)
  }
}

const print_OS = (x) => {
  display('', x)
  for (let i = 0; i < OS.length; i = i + 1) {
    const val = OS[i]
    display('', stringify(i) + ': ' + address_to_JS_value(val))
  }
}

// const run_vm = (jsonASTString) => {
//   const json = JSON.parse(jsonASTString)
//   compile_program(json)
//   return run()
// }
//for test
// let result = run_vm(
//   `{"tag":"blk","body":{"tag":"seq","stmts":[{"tag":"assmt","sym":[{"tag":"nam","sym":"x"}],"expr":[{"tag":"lit","val":0}]},{"tag":"assmt","sym":[{"tag":"nam","sym":"i"}],"expr":[{"tag":"lit","val":0}]},{"tag":"for","pred":{"tag":"binop","sym":"<","frst":{"tag":"nam","sym":"i"},"scnd":{"tag":"lit","val":100}},"body":{"tag":"blk","body":{"tag":"seq","stmts":[{"tag":"assmt","sym":[{"tag":"nam","sym":"j"}],"expr":[{"tag":"lit","val":0}]},{"tag":"for","pred":{"tag":"binop","sym":"<","frst":{"tag":"nam","sym":"j"},"scnd":{"tag":"lit","val":100}},"body":{"tag":"blk","body":{"tag":"seq","stmts":[{"tag":"assmt","sym":[{"tag":"nam","sym":"x"}],"expr":[{"tag":"binop","sym":"+","frst":{"tag":"binop","sym":"+","frst":{"tag":"nam","sym":"x"},"scnd":{"tag":"nam","sym":"i"}},"scnd":{"tag":"nam","sym":"j"}}]},{"tag":"assmt","sym":[{"tag":"nam","sym":"j"}],"expr":[{"tag":"binop","sym":"+","frst":{"tag":"nam","sym":"j"},"scnd":{"tag":"lit","val":1}}]}]}}},{"tag":"assmt","sym":[{"tag":"nam","sym":"i"}],"expr":[{"tag":"binop","sym":"+","frst":{"tag":"nam","sym":"i"},"scnd":{"tag":"lit","val":1}}]}]}}},{"tag":"nam","sym":"x"}]}}
// `
// )
// display(result)

// For connection with VMServer, currently cut to test VM easily
const run_vm = (jsonAST) => {
  //jsonAST是string
  const json = JSON.parse(jsonAST)
  compile_program(json)
  const result = run()
  display(result)
  return { result }
}
module.exports = run_vm
