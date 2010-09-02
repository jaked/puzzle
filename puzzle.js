// compiled by ocamlc 3.12.0, ocamljs 0.3
var ocamljs$caml_named_value = (function (){
var Match_failure$16g = "Match_failure";
var Out_of_memory$17g = "Out_of_memory";
var Stack_overflow$24g = "Stack_overflow";
var Invalid_argument$18g = "Invalid_argument";
var Failure$19g = "Failure";
var Not_found$20g = "Not_found";
var Sys_error$21g = "Sys_error";
var End_of_file$22g = "End_of_file";
var Division_by_zero$23g = "Division_by_zero";
var Sys_blocked_io$25g = "Sys_blocked_io";
var Assert_failure$26g = "Assert_failure";
var Undefined_recursive_module$27g = "Undefined_recursive_module";
/*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
 * Copyright (C) 2010 Jake Donham
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA
 */

var caml_blit_string = function (s1, o1, s2, o2, n) {
  for (var i = 0; i < n; i++)
    oc$$ssetu(s2, o2 + i, oc$$srefu(s1, o1 + i));
}
var caml_callback = function (f, a) { return _(f, [a]); }
var caml_callback2 = function (f, a1, a2) { return _(f, [a1, a2]); }
var caml_callback3 = function (f, a1, a2, a3) { return _(f, [a1, a2, a3]); }
var caml_callback4 = function (f, a1, a2, a3, a4) { return _(f, [a1, a2, a3, a4]); }
var caml_callback5 = function (f, a1, a2, a3, a4, a5) { return _(f, [a1, a2, a3, a4, a5]); }
var caml_callbackN = function (f, n, args) { return _(f, args); }
// XXX caml_callback_exn ?
var compare_val = function (v1, v2, total) {
  var LESS = -1;
  var GREATER = 1;
  var EQUAL = 0;
  var UNORDERED = -2; // XXX ok?

  // XXX needs some work

  if (v1 == v2 && total) return EQUAL;

  var t1 = typeof v1;
  var t2 = typeof v2;
  if (t1 == t2) {
    switch (t1) {
    case "boolean":
      if (v1 < v2) return LESS;
      if (v1 > v2) return GREATER;
      return EQUAL;
    case "number":
      if (v1 < v2) return LESS;
      if (v1 > v2) return GREATER;
      if (v1 != v2) {
	if (!total) return UNORDERED;
	if (v1 == v1) return GREATER;
	if (v2 == v2) return LESS;
	return EQUAL;
      }
      return EQUAL;
    case "string":
      if (v1 < v2) return LESS;
      if (v1 > v2) return GREATER;
      return EQUAL;
    case "function":
      caml_invalid_argument("equal: functional value");
    case "object":
      // like NaN
      if (v1 == null) {
	if (v2 == null) return EQUAL;
	return LESS;
      }
      if (v2 == null) return GREATER;

      // XXX is there a way to get the class of an object as a value?
      // XXX is it worth special casing various JS objects?
      if (v1 instanceof Date) {
	var t1 = v1.getTime();
	var t2 = v2.getTime();
	if (t1 < t2) return LESS;
	if (t1 > t2) return GREATER;
	return EQUAL;
      }
      if (v1 instanceof Array) {
	// we should always either have both tags or neither
	// so it is OK to fall through here
	if (v1.t < v2.t) return LESS;
	if (v1.t > v2.t) return GREATER;
	var sz1 = v1.length;
	var sz2 = v2.length;
	if (sz1 < sz2) return LESS;
	if (sz1 > sz2) return GREATER;
	if (sz1 == 0) return EQUAL;
	for (var i=0; i < sz1; i++)
	  {
	    var c = compare_val(v1[i], v2[i], total);
	    if (c != EQUAL) return c;
	  }
	return EQUAL;
      }
      if (v1 instanceof oc$$ms) {
	var s1 = v1.toString();
	var s2 = v2.toString();
	if (s1 < s2) return LESS;
	if (s1 > s2) return GREATER;
	return EQUAL;
      }
      if (v1._m != null && v2._m != null) { // i.e. an OCaml object XXX better test
        var oid1 = v1[1];
        var oid2 = v2[1];
        if (oid1 < oid2) return LESS;
        if (oid1 > oid2) return GREATER;
        return EQUAL;
      }
      return UNORDERED; // XXX
    default:
      return UNORDERED;
    }
  }

  // like NaN
  if (v1 == null) {
    if (v2 == null) return EQUAL;
    return LESS;
  }
  if (v2 == null) return GREATER;

  // one boolean and one int
  if (t1 == "boolean" || t2 == "boolean")
  {
    if (v1 < v2) return LESS;
    if (v1 > v2) return GREATER;
    return EQUAL;
  }
  // one mutable and one immutable string
  if (t1 == "string" || t2 == "string")
  {
    var s1 = v1.toString();
    var s2 = v2.toString();
    if (s1 < s2) return LESS;
    if (s1 > s2) return GREATER;
    return EQUAL;
  }
  // one constructor without data (number) and one with (object Array)
  if (t1 == "number") return LESS;
  if (t2 == "number") return GREATER;
  return UNORDERED;
}
var caml_compare = function (v1, v2) {
  var res = compare_val(v1, v2, 1);
  return res < 0 ? -1 : res > 0 ? 1 : 0;
}
var caml_equal = function (v1, v2) { return compare_val(v1, v2, 0) == 0; }
var caml_failwith = function (s) { throw $(Failure$19g, s); }
var caml_fill_string = function(s, o, l, c) {
  for (var i = 0; i < l; i++)
    oc$$ssetu(s, o + i, c);
}
var caml_float_compare = function (v1, v2) {
  if (v1 === v2) return 0;
  if (v1 < v2) return -1;
  if (v1 > v2) return 1;
  if (v1 === v1) return 1;
  if (v2 === v2) return -1;
  return 0;
}
var caml_float_of_string = function (s) {
  var f = parseFloat(s);
  return isNaN(f) ? caml_failwith("float_of_string") : f;
}
var caml_classify_float = function (f) {
  if (isNaN(f)) return 4; // FP_nan
  else if (!isFinite(f)) return 3; // FP_infinite
  else if (f === 0) return 2; // FP_zero
  // can't determine subnormal from js afaik
  else return 0; // FP_normal
}

var caml_greaterthan = function (v1, v2) { return compare_val(v1, v2, 0) > 0; }
var caml_greaterequal = function (v1, v2) { return compare_val(v1, v2, 0) >= 0; }
var caml_hash_univ_param = function (count, limit, obj) {
  // globals
  hash_univ_limit = limit;
  hash_univ_count = count;
  hash_accu = 0;

  // XXX needs work
  function hash_aux(obj) {
    hash_univ_limit--;
    if (hash_univ_count < 0 || hash_univ_limit < 0) return;

    function combine(n) { hash_accu = hash_accu * 65599 + n; }
    function combine_small(n) { hash_accu = hash_accu * 19 + n; }

    switch (typeof obj) {
    case "number":
      // XXX for floats C impl examines bit rep
      // XXX for constructors without data C impl uses combine_small
      hash_univ_count--;
      combine(obj);
      break;
    case "string":
      hash_univ_count--;
      for (var i = obj.length; i > 0; i--)
        combine_small(obj.charCodeAt(i));
      break;
    case "boolean":
      hash_univ_count--;
      combine_small(obj ? 1 : 0);
      break;
    case "object":
      if (obj instanceof oc$$ms)
        hash_aux(obj.toString());
      else if (obj instanceof Array) { // possibly a block
        if (obj.t) {
          hash_univ_count--;
          combine_small(obj.t);
          for (var i = obj.length; i > 0; i--)
            hash_aux(obj[i]);
        }
      }
      else if (obj._m != null) { // OCaml object, use oid
        hash_univ_count--;
        combine(obj[1]);
      }
      break;
    default:
      break;
    }
  }

  hash_aux(obj);
  return hash_accu & 0x3FFFFFFF;
}
var caml_input_value = function () { throw "caml_input_value"; }
var caml_input_value_from_string = function () { throw "caml_input_value_from_string"; }
var caml_install_signal_handler = function () { throw "caml_install_signal_handler"; }
var caml_int_compare = function (i1, i2) { return (i1 > i2) - (i1 < i2); }
var caml_int32_compare = function (i1, i2) { return (i1 > i2) - (i1 < i2); }
var caml_int64_compare = function (i1, i2) { throw "caml_int64_compare"; }
var caml_int64_float_of_bits = function (s) {
  // see pervasives.ml; int64s are represented by strings
  switch (s) {
  case "9218868437227405312": return Number.POSITIVE_INFINITY;
  case "-4503599627370496": return Number.NEGATIVE_INFINITY;
  case "9218868437227405313": return Number.NaN;
  case "9218868437227405311" : return Number.MAX_VALUE;
  case "4503599627370496": return Number.MIN_VALUE;
  case "4372995238176751616": return 0; // XXX how to get epsilon in js?
  default: return 0;
  }
}
var caml_int_of_string = function (s) {
  var i = parseInt(s, 10);
  return isNaN(i) ? caml_failwith("int_of_string") : i;
}
var caml_int32_of_string = caml_int_of_string;
var caml_int64_of_string = caml_int_of_string;
var caml_nativeint_of_string = caml_int_of_string;
var caml_invalid_argument = function (s) { throw $(Invalid_argument$18g, s); }
var caml_is_printable = function (c) { return c > 31 && c < 127; } // XXX get this right
var caml_lessthan = function (v1, v2) { return compare_val(v1, v2, 0) -1 < -1; }
var caml_lessequal = function (v1, v2) { return compare_val(v1, v2, 0) -1 <= -1; }
var caml_make_vect = function (l, i) {
  var a = new Array(l);
  for (var j = 0; j < l; j++)
    a[j] = i;
  return a;
}
var caml_marshal_data_size = function () { throw "caml_marshal_data_size"; }
var caml_md5_chan = function () { throw "caml_md5_chan"; }
var caml_md5_string = function () { throw "caml_md5_string"; }
var caml_ml_channel_size = function () { throw "caml_ml_channel_size"; }
var caml_ml_channel_size_64 = function () { throw "caml_ml_channel_size_64"; }
var caml_ml_close_channel = function () { throw "caml_ml_close_channel"; }

var caml_ml_flush = function (c) { }

var caml_ml_input = function () { throw "caml_ml_input"; }
var caml_ml_input_char = function () { throw "caml_ml_input_char"; }
var caml_ml_input_int = function () { throw "caml_ml_input_int"; }
var caml_ml_input_scan_line = function () { throw "caml_ml_input_scan_line"; }
var caml_ml_open_descriptor_in = function () { return 0; } // XXX
var caml_ml_open_descriptor_out = function () { return 0; } // XXX
var caml_ml_out_channels_list = function () { return 0; }

var caml_ml_output = function (c, b, s, l) { print_verbatim(b); }
var caml_ml_output_char = function (c, ch) {  }

var caml_ml_output_int = function () { throw "caml_ml_output_int"; }
var caml_ml_pos_in = function () { throw "caml_ml_pos_in"; }
var caml_ml_pos_in_64 = function () { throw "caml_ml_pos_in_64"; }
var caml_ml_pos_out = function () { throw "caml_ml_pos_out"; }
var caml_ml_pos_out_64 = function () { throw "caml_ml_pos_out_64"; }
var caml_ml_seek_in = function () { throw "caml_ml_seek_in"; }
var caml_ml_seek_in_64 = function () { throw "caml_ml_seek_in_64"; }
var caml_ml_seek_out = function () { throw "caml_ml_seek_out"; }
var caml_ml_seek_out_64 = function () { throw "caml_ml_seek_out_64"; }
var caml_ml_set_binary_mode = function () { throw "caml_ml_set_binary_mode"; }
var caml_named_value = function (n) { return oc$$nv[n]; }
var caml_nativeint_compare = function (i1, i2) { return (i1 > i2) - (i1 < i2); }
var caml_notequal = function (v1, v2) { return compare_val(v1, v2, 0) != 0; }
var caml_obj_dup = function (a) {
  var l = a.length;
  var d = new Array(l);
  for (var i=0; i < l; i++)
    d[i] = a[i];
  d.t = a.t;
  return d;
}
var caml_obj_is_block = function (o) { return !(typeof o == 'number') }
var caml_obj_tag = function(o) { return o.t || 0; }
var caml_obj_set_tag = function(o, t) { o.t = t; }
var caml_obj_block = function(t, s) { if (s == 0) return t; else { var a = new Array(s); a.t = t; return a; } }
var caml_obj_truncate = function(o, s) { o.length = s; }
var caml_output_value = function () { throw "caml_output_value"; }
var caml_output_value_to_string = function () { throw "caml_output_value_to_string"; }
var caml_output_value_to_buffer = function () { throw "caml_output_value_to_buffer"; }
var caml_record_backtrace = function () { throw "caml_record_backtrace"; }
var caml_backtrace_status = function () { throw "caml_backtrace_status"; }
var caml_get_exception_backtrace = function () { throw "caml_get_exception_backtrace"; }
var caml_register_named_value = function (n, v) { oc$$nv[n] = v; }
var caml_string_compare = function (s1, s2) {
  if (oc$$slt(s1, s2)) return -1;
  else if (oc$$sgt(s1, s2)) return 1;
  else return 0;
}
var caml_sys_exit = function () { throw "caml_sys_exit"; }
  var init_time = (new Date()).getTime() / 1000;
var caml_sys_time = function () { return (new Date()).getTime() / 1000 - init_time; }
var caml_sys_get_argv = function () { return $("", $()); } // XXX put something here?
var caml_sys_get_config = function () { return $("js", 32); } // XXX browser name?
var caml_sys_open = function () { throw "caml_sys_open"; }
var caml_sys_random_seed = function() { throw "caml_sys_random_seed"; }

// lexing.c

function Short(tbl, n) {
  var s = tbl.charCodeAt(n * 2) + (tbl.charCodeAt(n * 2 + 1) << 8);
  return s & 32768 ? s + -65536 : s;
}

var caml_lex_engine = function (tbl, start_state, lexbuf)
{
  var state, base, backtrk, c;

  state = start_state;
  if (state >= 0) {
    /* First entry */
    lexbuf[6] = lexbuf[4] = lexbuf[5];
    lexbuf[7] = -1;
  } else {
    /* Reentry after refill */
    state = -state - 1;
  }
  while(1) {
    /* Lookup base address or action number for current state */
    base = Short(tbl[0], state);
    if (base < 0) return -base-1;
    /* See if it's a backtrack point */
    backtrk = Short(tbl[1], state);
    if (backtrk >= 0) {
      lexbuf[6] = lexbuf[5];
      lexbuf[7] = backtrk;
    }
    /* See if we need a refill */
    if (lexbuf[5] >= lexbuf[2]){
      if (lexbuf[8] === false){
        return -state - 1;
      }else{
        c = 256;
      }
    }else{
      /* Read next input char */
      c = lexbuf[1].charCodeAt(lexbuf[5]);
      lexbuf[5] += 1;
    }
    /* Determine next state */
    if (Short(tbl[4], base + c) == state)
      state = Short(tbl[3], base + c);
    else
      state = Short(tbl[2], state);
    /* If no transition on this char, return to last backtrack point */
    if (state < 0) {
      lexbuf[5] = lexbuf[6];
      if (lexbuf[7] == -1) {
        caml_failwith("lexing: empty token");
      } else {
        return lexbuf[7];
      }
    }else{
      /* Erase the EOF condition only if the EOF pseudo-character was
         consumed by the automaton (i.e. there was no backtrack above)
       */
      if (c == 256) lexbuf[8] = false;
    }
  }
}

/***********************************************/
/* New lexer engine, with memory of positions  */
/***********************************************/

function run_mem(p, pc, mem, curr_pos) {
  for (;;) {
    var dst, src ;

    dst = p.charCodeAt(pc++) ;
    if (dst == 0xff)
      return ;
    src = p.charCodeAt(pc++) ;
    if (src == 0xff) {
      /*      fprintf(stderr,"[%hhu] <- %d\n",dst,Int_val(curr_pos)) ;*/
      mem[dst] = curr_pos ;
    } else {
      /*      fprintf(stderr,"[%hhu] <- [%hhu]\n",dst,src) ; */
      mem[dst] = mem[src] ;
    }
  }
}

function run_tag(p, pc, mem) {
  for (;;) {
    var dst, src ;

    dst = p.charCodeAt(pc++) ;
    if (dst == 0xff)
      return ;
    src = p.charCodeAt(pc++) ;
    if (src == 0xff) {
      /*      fprintf(stderr,"[%hhu] <- -1\n",dst) ; */
      mem[dst] = -1 ;
    } else {
      /*      fprintf(stderr,"[%hhu] <- [%hhu]\n",dst,src) ; */
      mem[dst] = mem[src] ;
    }
  }
}

var caml_new_lex_engine = function (tbl, start_state, lexbuf)
{
  var state, base, backtrk, c, pstate ;
  state = start_state;
  if (state >= 0) {
    /* First entry */
    lexbuf[6] = lexbuf[4] = lexbuf[5];
    lexbuf[7] = -1;
  } else {
    /* Reentry after refill */
    state = -state - 1;
  }
  while(1) {
    /* Lookup base address or action number for current state */
    base = Short(tbl[0], state);
    if (base < 0) {
      var pc_off = Short(tbl[5], state) ;
      run_tag(tbl[10], pc_off, lexbuf[9]);
      /*      fprintf(stderr,"Perform: %d\n",-base-1) ; */
      return -base-1;
    }
    /* See if it's a backtrack point */
    backtrk = Short(tbl[1], state);
    if (backtrk >= 0) {
      var pc_off =  Short(tbl[6], state);
      run_tag(tbl[10], pc_off, lexbuf[9]);
      lexbuf[6] = lexbuf[5];
      lexbuf[7] = backtrk;

    }
    /* See if we need a refill */
    if (lexbuf[5] >= lexbuf[2]){
      if (lexbuf[8] === false){
        return -state - 1;
      }else{
        c = 256;
      }
    }else{
      /* Read next input char */
      c = lexbuf[1].charCodeAt(lexbuf[5]);
      lexbuf[5] += 1;
    }
    /* Determine next state */
    pstate=state ;
    if (Short(tbl[4], base + c) == state)
      state = Short(tbl[3], base + c);
    else
      state = Short(tbl[2], state);
    /* If no transition on this char, return to last backtrack point */
    if (state < 0) {
      lexbuf[5] = lexbuf[6];
      if (lexbuf[7] == -1) {
        caml_failwith("lexing: empty token");
      } else {
        return lexbuf[7];
      }
    }else{
      /* If some transition, get and perform memory moves */
      var base_code = Short(tbl[5], pstate) ;
      var pc_off ;
      if (Short(tbl[9], base_code + c) == pstate)
        pc_off = Short(tbl[8], base_code + c) ;
      else
        pc_off = Short(tbl[7], pstate) ;
      if (pc_off > 0) 
        run_mem(tbl[10], pc_off, lexbuf[9], lexbuf[5]) ;
      /* Erase the EOF condition only if the EOF pseudo-character was
         consumed by the automaton (i.e. there was no backtrack above)
       */
      if (c == 256) lexbuf[8] = false;
    }
  }
}

// parsing.c

var caml_parser_trace = false

/* Auxiliary for printing token just read */

function token_name(names, number)
{
  var n = 0;
  for (/*nothing*/; number > 0; number--) {
    var i = names.indexOf("\x00", n);
    if (i == -1) return "<unknown token>";
    n = i + 1;
  }
  return names.substr(n, names.indexOf("\x00", n) - n);
}

function print_token(tables, state, tok)
{
  if (typeof tok == 'number') {
    print("State " + state + ": read token " + token_name(tables[14], tok));
  } else {
    print("State " + state + ": read token " + token_name(tables[15], tok.t) + "(" + tok[0] + ")");
  }      
}      

/* The pushdown automata */

var caml_parse_engine = function (tables, env, cmd, arg)
{
  var state;
  var sp, asp;
  var errflag;
  var n, n1, n2, m, state1;

  loop: while (true) switch (cmd) {

  case 0:
    state = 0;
    sp = env[13];
    errflag = 0;

  case -1:
    n = Short(tables[5], state);
    if (n != 0) { cmd = -7; continue loop; }
    if (env[6] >= 0) { cmd = -2; continue loop; }
    env[13] = sp; env[14] = state; env[15] = errflag;
    return 0;
                                /* The ML code calls the lexer and updates */
                                /* symb_start and symb_end */
  case 1:
    sp = env[13]; state = env[14]; errflag = env[15];
    if (!(typeof arg == 'number')) {
      env[6] = tables[2][arg.t];
      env[7] = arg[0];
    } else {
      env[6] = tables[1][arg];
      env[7] = 0;
    }
    if (caml_parser_trace) print_token(tables, state, arg);
    
  case -2:
    n1 = Short(tables[7], state);
    n2 = n1 + env[6];
    if (n1 != 0 && n2 >= 0 && n2 <= tables[10] &&
        Short(tables[12], n2) == env[6]) { cmd = -4; continue loop; }
    n1 = Short(tables[8], state);
    n2 = n1 + env[6];
    if (n1 != 0 && n2 >= 0 && n2 <= tables[10] &&
        Short(tables[12], n2) == env[6]) {
      n = Short(tables[11], n2);
      cmd = -7; continue loop;
    }
    if (errflag > 0) { cmd = -3; continue; }
    env[13] = sp; env[14] = state; env[15] = errflag;
    return 5;
                                /* The ML code calls the error function */
  case 5:
    sp = env[13]; state = env[14]; errflag = env[15];
  case -3:
    if (errflag < 3) {
      errflag = 3;
      while (1) {
        state1 = env[0][sp];
        n1 = Short(tables[7], state1);
        n2 = n1 + 256;
        if (n1 != 0 && n2 >= 0 && n2 <= tables[10] &&
            Short(tables[12], n2) == 256) {
          if (caml_parser_trace) 
            print("Recovering in state " + state1);
          cmd = -5; continue loop;
        } else {
          if (caml_parser_trace){
            print("Discarding state " + state1);
          }
          if (sp <= env[5]) {
            if (caml_parser_trace){
              print("No more states to discard");
            }
            return 1; /* The ML code raises Parse_error */
          }
          sp--;
        }
      }
    } else {
      if (env[6] == 0)
        return 1; /* The ML code raises Parse_error */
      if (caml_parser_trace) print("Discarding last token read");
      env[6] = -1;
      cmd = -1; continue loop;
    }
    
  case -4:
    env[6] = -1;
    if (errflag > 0) errflag--;
  case -5:
    if (caml_parser_trace)
      print("State " + state + ": shift to state " + Short(tables[11], n2));
    state = Short(tables[11], n2);
    sp++;
    if (sp < env[4]) { cmd = -6; continue loop; }
    env[13] = sp; env[14] = state; env[15] = errflag;
    return 2;
                                 /* The ML code resizes the stacks */
  case 2:
    sp = env[13]; state = env[14]; errflag = env[15];
  case -6:
    env[0][sp] = state;
    env[1][sp] = env[7];
    env[2][sp] = env[8];
    env[3][sp] = env[9];
    cmd = -1; continue loop;

  case -7:
    if (caml_parser_trace)
      print("State " + state + ": reduce by rule " + n);
    m = Short(tables[4], n);
    env[10] = sp;
    env[12] = n;
    env[11] = m;
    sp = sp - m + 1;
    m = Short(tables[3], n);
    state1 = env[0][sp - 1];
    n1 = Short(tables[9], m);
    n2 = n1 + state1;
    if (n1 != 0 && n2 >= 0 && n2 <= tables[10] &&
        Short(tables[12], n2) == state1) {
      state = Short(tables[11], n2);
    } else {
      state = Short(tables[6], m);
    }
    if (sp < env[4]) { cmd = -8; continue loop; }
    env[13] = sp; env[14] = state; env[15] = errflag;
    return 3;
                                /* The ML code resizes the stacks */
  case 3:
    sp = env[13]; state = env[14]; errflag = env[15];
  case -8:
    env[13] = sp; env[14] = state; env[15] = errflag;
    return 4;
                                /* The ML code calls the semantic action */
  case 4:
    sp = env[13]; state = env[14]; errflag = env[15];
    env[0][sp] = state;
    env[1][sp] = arg;
    asp = env[10];
    env[3][sp] = env[3][asp];
    if (sp > asp) {
      /* This is an epsilon production. Take symb_start equal to symb_end. */
      env[2][sp] = env[3][asp];
    }
    cmd = -1; continue loop;
  }
}

var caml_set_parser_trace = function (flag)
{
  var oldflag = caml_parser_trace;
  caml_parser_trace = flag;
  return oldflag;
}

/*
  stuff below taken from js_of_ocaml/lib
  Copyright (C) 2010 Jérôme Vouillon
*/

///////////// Format
//Provides: caml_parse_format
//Requires: caml_invalid_argument
function caml_parse_format (fmt) {
  fmt = fmt.toString ();
  var len = fmt.length;
  if (len > 31) caml_invalid_argument("format_int: format too long");
  var f =
    { justify:'+', signstyle:'-', filler:' ', alternate:false,
      base:0, signedconv:false, width:0, uppercase:false,
      sign:1, prec:6, conv:'f' };
  for (var i = 0; i < len; i++) {
    var c = fmt.charAt(i);
    switch (c) {
    case '-':
      f.justify = '-'; break;
    case '+': case ' ':
      f.signstyle = c; break;
    case '0':
      f.filler = '0'; break;
    case '#':
      f.alternate = true; break;
    case '1': case '2': case '3': case '4': case '5':
    case '6': case '7': case '8': case '9':
      f.width = 0;
      while (c=fmt.charCodeAt(i) - 48, c >= 0 && c <= 9) {
        f.width = f.width * 10 + c; i++
      }
      i--;
     break;
    case '.':
      f.prec = 0;
      i++;
      while (c=fmt.charCodeAt(i) - 48, c >= 0 && c <= 9) {
        f.prec = f.prec * 10 + c; i++
      }
      i--;
    case 'd': case 'i': case 'l': case 'n': case 'L': case 'N':
      f.signedconv = true; /* fallthrough */
    case 'u':
      f.base = 10; break;
    case 'x':
      f.base = 16; break;
    case 'X':
      f.base = 16; f.uppercase = true; break;
    case 'o':
      f.base = 8; break;
    case 'e': case 'f': case 'g':
      f.signedconv = true; f.conv = c; break;
    case 'E': case 'F': case 'G':
      f.signedconv = true; f.uppercase = true;
      f.conv = c.toLowerCase (); break;
    }
  }
  return f;
}

//Provides: caml_finish_formatting
//Requires: MlString
function caml_finish_formatting(f, rawbuffer) {
  if (f.uppercase) rawbuffer = rawbuffer.toUpperCase();
  var len = rawbuffer.length;
  /* Adjust len to reflect additional chars (sign, etc) */
  if (f.signedconv && (f.sign < 0 || f.signstyle != '-')) len++;
  if (f.alternate) {
    if (f.base == 8) len += 1;
    if (f.base == 16) len += 2;
  }
  /* Do the formatting */
  var buffer = "";
  if (f.justify == '+' && f.filler == ' ')
    for (i = len; i < f.width; i++) buffer += ' ';
  if (f.signedconv) {
    if (f.sign < 0) buffer += '-';
    else if (f.signstyle != '-') buffer += f.signstyle;
  }
  if (f.alternate && f.base == 8) buffer += '0';
  if (f.alternate && f.base == 16) buffer += "0x";
  if (f.justify == '+' && f.filler == '0')
    for (i = len; i < f.width; i++) buffer += '0';
  buffer += rawbuffer;
  if (f.justify == '-')
    for (i = len; i < f.width; i++) buffer += ' ';
  return buffer;
}

//Provides: caml_format_int const
//Requires: caml_parse_format, caml_finish_formatting
function caml_format_int(fmt, i) {
  if (fmt.toString() == "%d") return (""+i);
  var f = caml_parse_format(fmt);
  if (i < 0) { if (f.signedconv) { f.sign = -1; i = -i; } else i >>>= 0; }
  var s = i.toString(f.base);
  return caml_finish_formatting(f, s);
}

//Provides: caml_format_float const
//Requires: caml_parse_format, caml_finish_formatting
function caml_format_float (fmt, x) {
  var s, f = caml_parse_format(fmt);
  if (x < 0) { f.sign = -1; x = -x; }
  if (isNaN(x)) { s = "nan"; f.filler = ' '; }
  else if (!isFinite(x)) { s = "inf"; f.filler = ' '; }
  else
    switch (f.conv) {
    case 'e':
      var s = x.toExponential(f.prec);
      // exponent should be at least two digits
      var i = s.length;
      if (s.charAt(i - 3) == 'e')
        s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
      break;
    case 'f':
      s = x.toFixed(f.prec); break;
    case 'g':
      var prec = f.prec?f.prec:1;
      s = x.toExponential(prec - 1);
      var j = s.indexOf('e');
      var exp = +s.slice(j + 1);
      if (exp < -4 || x.toFixed(0).length > prec) {
        // remove trailing zeroes
        var i = j - 1; while (s.charAt(i) == '0') i--;
        if (s.charAt(i) == '.') i--;
        s = s.slice(0, i + 1) + s.slice(j);
        i = s.length;
        if (s.charAt(i - 3) == 'e')
          s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
        break;
      } else {
        var p = prec;
        if (exp < 0) { p -= exp + 1; s = x.toFixed(p); }
        else while (s = x.toFixed(p), s.length > prec + 1) p--;
        if (p) {
          // remove trailing zeroes
          i = s.length - 1; while (s.charAt(i) == '0') i--;
          if (s.charAt(i) == '.') i--;
          s = s.slice(0, i + 1);
        }
      }
      break;
    }
  return caml_finish_formatting(f, s);
}
/*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
 * Copyright (C) 2010 Jake Donham
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA
 */

/*
function console_log(s) {
  var cs = Components.classes["@mozilla.org/consoleservice;1"].getService(Components.interfaces["nsIConsoleService"]);
  cs.logStringMessage(s);
}
*/

var oc$$nv = {}

// XXX name these sensibly and compactify code afterwards

function ___a(m, t, a) {
  return m.apply(t, a);
}

/*@cc_on @if (@_win32 && @_jscript_version >= 5)
function ___a(m, t, a) {
  if (m.apply)
    return m.apply(t, a);
  else
    // IE < 8 doesn't support apply for DOM methods, but does support "cached" methods bound to an object
    switch (a.length) {
    case 0: return m();
    case 1: return m(a[0]);
    case 2: return m(a[0], a[1]);
    case 3: return m(a[0], a[1], a[2]);
    case 4: return m(a[0], a[1], a[2], a[3]);
    case 5: return m(a[0], a[1], a[2], a[3], a[4]);
    case 6: return m(a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7: return m(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    default: throw "unimplemented";
    }
}
@end @*/

function ___m(m, t, a)
{
  function ap(a1, a2) {
    var a = new Array();
    for (var i=0; i < a1.length; i++) a.push(a1[i]);
    for (var i=0; i < a2.length; i++) a.push(a2[i]);
    return a;
  }

  while (true) {
    var al = a.length;
    var ml = m.length;

    if (al < ml)
    {
      switch (ml - al) {
      case 1: return _f(function (z) { return m.apply(t, ap(a, arguments)) });
      case 2: return _f(function (z,y) { return m.apply(t, ap(a, arguments)) });
      case 3: return _f(function (z,y,x) { return m.apply(t, ap(a, arguments)) });
      case 4: return _f(function (z,y,x,w) { return m.apply(t, ap(a, arguments)) });
      case 5: return _f(function (z,y,x,w,v) { return m.apply(t, ap(a, arguments)) });
      case 6: return _f(function (z,y,x,w,v,u) { return m.apply(t, ap(a, arguments)) });
      case 7: return _f(function (z,y,x,w,v,u,s) { return m.apply(t, ap(a, arguments)) });
      default: throw "unimplemented";
      }
    }
    else if (al == ml)
      return m.apply(t, a);
    else // al > ml
    {
      m = _m(m, t, a.slice(0, ml));
      t = m;
      a = a.slice(ml);
    }
  }
}

var $in_tail = false;

// tail call
function __m(m, t, args)
{
  if (m.$oc) {
    if ($in_tail) {
      args.$m = m;
      args.$t = t;
      args.$tr = true;
      return args;
    }
    else
      return _m(m, t, args);
  }
  else {
    var old_in_tail = $in_tail;
    $in_tail = false;
    try { return ___a(m, t, args); }
    finally { $in_tail = old_in_tail; }
  }
}
function __(t, args) { return __m(t, t, args); }

// non tail call
function _m(m, t, args)
{
  if (m.$oc) {
    var old_in_tail = $in_tail;
    $in_tail = true;
    try {
      var v = __m(m, t, args);
      while (v && v.$tr)
        v = ___m(v.$m, v.$t, v);
      return v;
    }
    finally { $in_tail = old_in_tail; }
  }
  else {
    var old_in_tail = $in_tail;
    $in_tail = false;
    try { return ___a(m, t, args); }
    finally { $in_tail = old_in_tail; }
  }
}
function _(t, args) { return _m(t, t, args); }

function _f(f) {
  f.$oc = true;
  return f;
}

function $N(t, a) {
  var l = a.length;
  var b = new Array(l);
  for (var i=0; i < l; i++)
    b[i] = a[i];
  b.t = t;
  return b;
}
function $() { return $N(0, arguments); }
function $1() { return $N(1, arguments); }
function $2() { return $N(2, arguments); }
function $3() { return $N(3, arguments); }
function $4() { return $N(4, arguments); }
function $5() { return $N(5, arguments); }
function $6() { return $N(6, arguments); }
function $7() { return $N(7, arguments); }
function $8() { return $N(8, arguments); }
function $9() { return $N(9, arguments); }
function $t(a) { return a.t; }

function $xM(t) { return { $t: t }; }
function $xN(t, a) { a.$t = t; return a; }
function $xt(a) { return a.$t; }

function oc$$arefs(o, i) {
  return i < o.length ? o[i] : oc$Pervasives$[0]("index out of bounds");
}
function oc$$asets(o, i, v) {
  return i < o.length ? o[i] = v : oc$Pervasives$[0]("index out of bounds");
}

// mutable strings, argh

function oc$$ms(a) {
  this.a = a;
  this.length = a.length;
}

// XXX cache the string rep?
oc$$ms.prototype.toString = function () { return String.fromCharCode.apply(null, this.a); }

function oc$$lms(s) {
  var l = s.length;
  var a = new Array(l);
  for (var i = 0; i < l; i++)
    a[i] = s.charCodeAt(i);
  return new oc$$ms(a);
}
function oc$$cms(n) {
  return new oc$$ms(new Array(n));
}
function oc$$srefu(o, i) { return typeof o == "string" ? o.charCodeAt(i) : o.a[i]; }
function oc$$ssetu(o, i, v) { o.a[i] = v; }
function oc$$srefs(o, i) {
  return i < o.length ? oc$$srefu(o, i) : oc$Pervasives$[0]("index out of bounds");
}
function oc$$ssets(o, i, v) {
  return i < o.length ? oc$$ssetu(o, i, v) : oc$Pervasives$[0]("index out of bounds");
}

function oc$$seq(s1, s2) { return s1.toString() == s2.toString(); }
function oc$$sneq(s1, s2) { return s1.toString() != s2.toString(); }
function oc$$slt(s1, s2) { return s1.toString() < s2.toString(); }
function oc$$sgt(s1, s2) { return s1.toString() > s2.toString(); }
function oc$$slte(s1, s2) { return s1.toString() <= s2.toString(); }
function oc$$sgte(s1, s2) { return s1.toString() >= s2.toString(); }

/*@cc_on @if (@_win32 && @_jscript_version >= 5) if (!window.XMLHttpRequest)
window.XMLHttpRequest = function() { return new ActiveXObject('Microsoft.XMLHTTP') };
@end @*/
var oc$Pervasives$ =
  function () {
    var failwith$1026 = _f(function (s$1027) { throw $(Failure$19g, s$1027); });
    var invalid_arg$1028 = _f(function (s$1029) { throw $(Invalid_argument$18g, s$1029); });
    var Exit$1030 = $("Pervasives.Exit");
    var min$1038 = _f(function (x$1039, y$1040) { if (caml_lessequal(x$1039, y$1040)) return x$1039; return y$1040; });
    var max$1041 = _f(function (x$1042, y$1043) { if (caml_greaterequal(x$1042, y$1043)) return x$1042; return y$1043; });
    var abs$1060 = _f(function (x$1061) { if (x$1061 >= 0) return x$1061; return -x$1061; });
    var lnot$1065 = _f(function (x$1066) { return x$1066 ^ -1; });
    var min_int$1070 = 1 << (1 << 31 === 0 ? 30 : 62);
    var max_int$1071 = min_int$1070 - 1;
    var infinity$1107 = caml_int64_float_of_bits("9218868437227405312");
    var neg_infinity$1108 = caml_int64_float_of_bits("-4503599627370496");
    var nan$1109 = caml_int64_float_of_bits("9218868437227405313");
    var max_float$1110 = caml_int64_float_of_bits("9218868437227405311");
    var min_float$1111 = caml_int64_float_of_bits("4503599627370496");
    var epsilon_float$1112 = caml_int64_float_of_bits("4372995238176751616");
    var $5E$1128 = _f(function (s1$1129, s2$1130) { return s1$1129.toString() + s2$1130.toString(); });
    var char_of_int$1133 =
      _f(function (n$1134) { if (n$1134 < 0 || n$1134 > 255) return __(invalid_arg$1028, [ "char_of_int" ]); return n$1134; });
    var string_of_bool$1140 = _f(function (b$1141) { if (b$1141) return "true"; return "false"; });
    var bool_of_string$1142 =
      _f(function (param$1404) {
           if (!oc$$sneq(param$1404, "false")) return false;
           if (oc$$sneq(param$1404, "true")) return __(invalid_arg$1028, [ "bool_of_string" ]);
           return true;
         });
    var string_of_int$1143 = _f(function (n$1144) { return caml_format_int("%d", n$1144); });
    var String$1147 = $();
    var valid_float_lexem$1148 =
      _f(function (s$1149) {
           var l$1150 = s$1149.length;
           var loop$1151 =
             _f(function (i$1152) {
                  if (i$1152 >= l$1150) return __($5E$1128, [ s$1149, "." ]);
                  var match$1403 = oc$$srefs(s$1149, i$1152);
                  var $r58 = false;
                  r$58: {
                    {
                      if (!(match$1403 >= 48)) { { if (!(match$1403 !== 45)) { { $r58 = true; break r$58; } } return s$1149; } }
                      if (!(match$1403 >= 58)) { { $r58 = true; break r$58; } }
                      return s$1149;
                    }
                  }
                  if ($r58) return __(loop$1151, [ i$1152 + 1 ]);
                });
           return __(loop$1151, [ 0 ]);
         });
    var string_of_float$1153 = _f(function (f$1154) { return __(valid_float_lexem$1148, [ caml_format_float("%.12g", f$1154) ]); });
    var $40$1156 =
      _f(function (l1$1157, l2$1158) { if (l1$1157) return $(l1$1157[0], _($40$1156, [ l1$1157[1], l2$1158 ])); return l2$1158; });
    var stdin$1165 = caml_ml_open_descriptor_in(0);
    var stdout$1166 = caml_ml_open_descriptor_out(1);
    var stderr$1167 = caml_ml_open_descriptor_out(2);
    var open_out_gen$1188 =
      _f(function (mode$1189, perm$1190, name$1191) {
           return caml_ml_open_descriptor_out(caml_sys_open(name$1191, mode$1189, perm$1190));
         });
    var open_out$1192 = _f(function (name$1193) { return __(open_out_gen$1188, [ $(1, $(3, $(4, $(7, 0)))), 438, name$1193 ]); });
    var open_out_bin$1194 =
      _f(function (name$1195) { return __(open_out_gen$1188, [ $(1, $(3, $(4, $(6, 0)))), 438, name$1195 ]); });
    var flush_all$1198 =
      _f(function (param$1400) {
           var iter$1199 =
             _f(function (param$1401) {
                  if (param$1401) {
                    { try { caml_ml_flush(param$1401[0]); } catch (exn$1402) { } return __(iter$1199, [ param$1401[1] ]); }
                  }
                  return 0;
                });
           return __(iter$1199, [ caml_ml_out_channels_list(0) ]);
         });
    var output_string$1204 = _f(function (oc$1205, s$1206) { return caml_ml_output(oc$1205, s$1206, 0, s$1206.length); });
    var output$1207 =
      _f(function (oc$1208, s$1209, ofs$1210, len$1211) {
           if (ofs$1210 < 0 || (len$1211 < 0 || ofs$1210 > s$1209.length - len$1211)) return __(invalid_arg$1028, [ "output" ]);
           return caml_ml_output(oc$1208, s$1209, ofs$1210, len$1211);
         });
    var output_value$1215 = _f(function (chan$1216, v$1217) { return caml_output_value(chan$1216, v$1217, 0); });
    var close_out$1222 = _f(function (oc$1223) { caml_ml_flush(oc$1223); return caml_ml_close_channel(oc$1223); });
    var close_out_noerr$1224 =
      _f(function (oc$1225) {
           try { caml_ml_flush(oc$1225); } catch (exn$1399) { }
           try { return caml_ml_close_channel(oc$1225); } catch (exn$1398) { return 0; }
         });
    var open_in_gen$1227 =
      _f(function (mode$1228, perm$1229, name$1230) {
           return caml_ml_open_descriptor_in(caml_sys_open(name$1230, mode$1228, perm$1229));
         });
    var open_in$1231 = _f(function (name$1232) { return __(open_in_gen$1227, [ $(0, $(7, 0)), 0, name$1232 ]); });
    var open_in_bin$1233 = _f(function (name$1234) { return __(open_in_gen$1227, [ $(0, $(6, 0)), 0, name$1234 ]); });
    var input$1237 =
      _f(function (ic$1238, s$1239, ofs$1240, len$1241) {
           if (ofs$1240 < 0 || (len$1241 < 0 || ofs$1240 > s$1239.length - len$1241)) return __(invalid_arg$1028, [ "input" ]);
           return caml_ml_input(ic$1238, s$1239, ofs$1240, len$1241);
         });
    var unsafe_really_input$1242 =
      _f(function (ic$1243, s$1244, ofs$1245, len$1246) {
           if (len$1246 <= 0) return 0;
           var r$1247 = caml_ml_input(ic$1243, s$1244, ofs$1245, len$1246);
           if (r$1247 === 0) throw $(End_of_file$22g);
           return __(unsafe_really_input$1242, [ ic$1243, s$1244, ofs$1245 + r$1247, len$1246 - r$1247 ]);
         });
    var really_input$1248 =
      _f(function (ic$1249, s$1250, ofs$1251, len$1252) {
           if (ofs$1251 < 0 || (len$1252 < 0 || ofs$1251 > s$1250.length - len$1252))
             return __(invalid_arg$1028, [ "really_input" ]);
           return __(unsafe_really_input$1242, [ ic$1249, s$1250, ofs$1251, len$1252 ]);
         });
    var input_line$1254 =
      _f(function (chan$1255) {
           var build_result$1256 =
             _f(function (buf$1257, pos$1258, param$1397) {
                  if (param$1397) {
                    {
                      var hd$1259 = param$1397[0];
                      var len$1261 = hd$1259.length;
                      caml_blit_string(hd$1259, 0, buf$1257, pos$1258 - len$1261, len$1261);
                      return __(build_result$1256, [ buf$1257, pos$1258 - len$1261, param$1397[1] ]);
                    }
                  }
                  return buf$1257;
                });
           var scan$1262 =
             _f(function (accu$1263, len$1264) {
                  var n$1265 = caml_ml_input_scan_line(chan$1255);
                  if (!(n$1265 === 0)) {
                    {
                      if (n$1265 > 0) {
                        {
                          var res$1266 = oc$$cms(n$1265 - 1);
                          caml_ml_input(chan$1255, res$1266, 0, n$1265 - 1);
                          caml_ml_input_char(chan$1255);
                          if (accu$1263) {
                            {
                              var len$1267 = len$1264 + n$1265 - 1;
                              return __(build_result$1256, [ oc$$cms(len$1267), len$1267, $(res$1266, accu$1263) ]);
                            }
                          }
                          return res$1266;
                        }
                      }
                      var beg$1268 = oc$$cms(-n$1265);
                      caml_ml_input(chan$1255, beg$1268, 0, -n$1265);
                      return __(scan$1262, [ $(beg$1268, accu$1263), len$1264 - n$1265 ]);
                    }
                  }
                  if (accu$1263) return __(build_result$1256, [ oc$$cms(len$1264), len$1264, accu$1263 ]);
                  throw $(End_of_file$22g);
                });
           return __(scan$1262, [ 0, 0 ]);
         });
    var close_in_noerr$1276 =
      _f(function (ic$1277) { try { return caml_ml_close_channel(ic$1277); } catch (exn$1396) { return 0; } });
    var print_char$1279 = _f(function (c$1280) { return caml_ml_output_char(stdout$1166, c$1280); });
    var print_string$1281 = _f(function (s$1282) { return __(output_string$1204, [ stdout$1166, s$1282 ]); });
    var print_int$1283 =
      _f(function (i$1284) { return __(output_string$1204, [ stdout$1166, _(string_of_int$1143, [ i$1284 ]) ]); });
    var print_float$1285 =
      _f(function (f$1286) { return __(output_string$1204, [ stdout$1166, _(string_of_float$1153, [ f$1286 ]) ]); });
    var print_endline$1287 =
      _f(function (s$1288) {
           _(output_string$1204, [ stdout$1166, s$1288 ]);
           caml_ml_output_char(stdout$1166, 10);
           return caml_ml_flush(stdout$1166);
         });
    var print_newline$1289 = _f(function (param$1395) { caml_ml_output_char(stdout$1166, 10); return caml_ml_flush(stdout$1166); });
    var prerr_char$1290 = _f(function (c$1291) { return caml_ml_output_char(stderr$1167, c$1291); });
    var prerr_string$1292 = _f(function (s$1293) { return __(output_string$1204, [ stderr$1167, s$1293 ]); });
    var prerr_int$1294 =
      _f(function (i$1295) { return __(output_string$1204, [ stderr$1167, _(string_of_int$1143, [ i$1295 ]) ]); });
    var prerr_float$1296 =
      _f(function (f$1297) { return __(output_string$1204, [ stderr$1167, _(string_of_float$1153, [ f$1297 ]) ]); });
    var prerr_endline$1298 =
      _f(function (s$1299) {
           _(output_string$1204, [ stderr$1167, s$1299 ]);
           caml_ml_output_char(stderr$1167, 10);
           return caml_ml_flush(stderr$1167);
         });
    var prerr_newline$1300 = _f(function (param$1394) { caml_ml_output_char(stderr$1167, 10); return caml_ml_flush(stderr$1167); });
    var read_line$1301 = _f(function (param$1393) { caml_ml_flush(stdout$1166); return __(input_line$1254, [ stdin$1165 ]); });
    var read_int$1302 = _f(function (param$1392) { return caml_int_of_string(_(read_line$1301, [ 0 ])); });
    var read_float$1303 = _f(function (param$1391) { return caml_float_of_string(_(read_line$1301, [ 0 ])); });
    var LargeFile$1310 = $();
    var $5E$5E$1325 = _f(function (fmt1$1326, fmt2$1327) { return _($5E$1128, [ fmt1$1326, _($5E$1128, [ "%,", fmt2$1327 ]) ]); });
    var string_of_format$1328 =
      _f(function (fmt$1329) {
           var s$1330 = fmt$1329;
           var l$1331 = s$1330.length;
           var r$1332 = oc$$cms(l$1331);
           caml_blit_string(s$1330, 0, r$1332, 0, l$1331);
           return r$1332;
         });
    var exit_function$1334 = $(flush_all$1198);
    var at_exit$1335 =
      _f(function (f$1336) {
           var g$1337 = exit_function$1334[0];
           return exit_function$1334[0] = _f(function (param$1390) { _(f$1336, [ 0 ]); return __(g$1337, [ 0 ]); });
         });
    var do_at_exit$1338 = _f(function (param$1389) { return __(exit_function$1334[0], [ 0 ]); });
    var exit$1339 = _f(function (retcode$1340) { _(do_at_exit$1338, [ 0 ]); return caml_sys_exit(retcode$1340); });
    caml_register_named_value("Pervasives.do_at_exit", do_at_exit$1338);
    return $(invalid_arg$1028, failwith$1026, Exit$1030, min$1038, max$1041, abs$1060, max_int$1071, min_int$1070, lnot$1065,
             infinity$1107, neg_infinity$1108, nan$1109, max_float$1110, min_float$1111, epsilon_float$1112, $5E$1128,
             char_of_int$1133, string_of_bool$1140, bool_of_string$1142, string_of_int$1143, string_of_float$1153, $40$1156,
             stdin$1165, stdout$1166, stderr$1167, print_char$1279, print_string$1281, print_int$1283, print_float$1285,
             print_endline$1287, print_newline$1289, prerr_char$1290, prerr_string$1292, prerr_int$1294, prerr_float$1296,
             prerr_endline$1298, prerr_newline$1300, read_line$1301, read_int$1302, read_float$1303, open_out$1192,
             open_out_bin$1194, open_out_gen$1188, _f(function (prim$1357) { return caml_ml_flush(prim$1357); }), flush_all$1198,
             _f(function (prim$1359, prim$1358) { return caml_ml_output_char(prim$1359, prim$1358); }), output_string$1204,
             output$1207, _f(function (prim$1361, prim$1360) { return caml_ml_output_char(prim$1361, prim$1360); }),
             _f(function (prim$1363, prim$1362) { return caml_ml_output_int(prim$1363, prim$1362); }), output_value$1215,
             _f(function (prim$1365, prim$1364) { return caml_ml_seek_out(prim$1365, prim$1364); }),
             _f(function (prim$1366) { return caml_ml_pos_out(prim$1366); }),
             _f(function (prim$1367) { return caml_ml_channel_size(prim$1367); }), close_out$1222, close_out_noerr$1224,
             _f(function (prim$1369, prim$1368) { return caml_ml_set_binary_mode(prim$1369, prim$1368); }), open_in$1231,
             open_in_bin$1233, open_in_gen$1227, _f(function (prim$1370) { return caml_ml_input_char(prim$1370); }),
             input_line$1254, input$1237, really_input$1248, _f(function (prim$1371) { return caml_ml_input_char(prim$1371); }),
             _f(function (prim$1372) { return caml_ml_input_int(prim$1372); }),
             _f(function (prim$1373) { return caml_input_value(prim$1373); }),
             _f(function (prim$1375, prim$1374) { return caml_ml_seek_in(prim$1375, prim$1374); }),
             _f(function (prim$1376) { return caml_ml_pos_in(prim$1376); }),
             _f(function (prim$1377) { return caml_ml_channel_size(prim$1377); }),
             _f(function (prim$1378) { return caml_ml_close_channel(prim$1378); }), close_in_noerr$1276,
             _f(function (prim$1380, prim$1379) { return caml_ml_set_binary_mode(prim$1380, prim$1379); }),
             $(_f(function (prim$1382, prim$1381) { return caml_ml_seek_out_64(prim$1382, prim$1381); }),
               _f(function (prim$1383) { return caml_ml_pos_out_64(prim$1383); }),
               _f(function (prim$1384) { return caml_ml_channel_size_64(prim$1384); }),
               _f(function (prim$1386, prim$1385) { return caml_ml_seek_in_64(prim$1386, prim$1385); }),
               _f(function (prim$1387) { return caml_ml_pos_in_64(prim$1387); }),
               _f(function (prim$1388) { return caml_ml_channel_size_64(prim$1388); })), string_of_format$1328, $5E$5E$1325,
             exit$1339, at_exit$1335, valid_float_lexem$1148, unsafe_really_input$1242, do_at_exit$1338);
  }();
var oc$Array$ =
  function () {
    var init$1037 =
      _f(function (l$1038, f$1039) {
           if (l$1038 === 0) return $();
           var res$1040 = caml_make_vect(l$1038, _(f$1039, [ 0 ]));
           for (var i$1041 = 1; i$1041 <= -1 + l$1038; i$1041++) {
             (function (i$1041) { res$1040[i$1041] = _(f$1039, [ i$1041 ]); }(i$1041));
           }
           return res$1040;
         });
    var make_matrix$1042 =
      _f(function (sx$1043, sy$1044, init$1045) {
           var res$1046 = caml_make_vect(sx$1043, $());
           for (var x$1047 = 0; x$1047 <= -1 + sx$1043; x$1047++) {
             (function (x$1047) { res$1046[x$1047] = caml_make_vect(sy$1044, init$1045); }(x$1047));
           }
           return res$1046;
         });
    var copy$1049 =
      _f(function (a$1050) {
           var l$1051 = a$1050.length;
           if (l$1051 === 0) return $();
           var res$1052 = caml_make_vect(l$1051, a$1050[0]);
           for (var i$1053 = 1; i$1053 <= -1 + l$1051; i$1053++) {
             (function (i$1053) { res$1052[i$1053] = a$1050[i$1053]; }(i$1053));
           }
           return res$1052;
         });
    var append$1054 =
      _f(function (a1$1055, a2$1056) {
           var l1$1057 = a1$1055.length;
           var l2$1058 = a2$1056.length;
           if (l1$1057 === 0 && l2$1058 === 0) return $();
           var r$1059 = caml_make_vect(l1$1057 + l2$1058, (l1$1057 > 0 ? a1$1055 : a2$1056)[0]);
           for (var i$1060 = 0; i$1060 <= l1$1057 - 1; i$1060++) {
             (function (i$1060) { r$1059[i$1060] = a1$1055[i$1060]; }(i$1060));
           }
           for (var i$1061 = 0; i$1061 <= l2$1058 - 1; i$1061++) {
             (function (i$1061) { r$1059[i$1061 + l1$1057] = a2$1056[i$1061]; }(i$1061));
           }
           return r$1059;
         });
    var concat_aux$1062 =
      _f(function (init$1063, al$1064) {
           var size$1065 =
             _f(function (accu$1066, param$1234) {
                  if (param$1234) return __(size$1065, [ accu$1066 + (param$1234[0]).length, param$1234[1] ]);
                  return accu$1066;
                });
           var res$1069 = caml_make_vect(_(size$1065, [ 0, al$1064 ]), init$1063);
           var fill$1070 =
             _f(function (pos$1071, param$1233) {
                  if (param$1233) {
                    {
                      var h$1072 = param$1233[0];
                      for (var i$1074 = 0; i$1074 <= h$1072.length - 1; i$1074++) {
                        (function (i$1074) { res$1069[pos$1071 + i$1074] = h$1072[i$1074]; }(i$1074));
                      }
                      return __(fill$1070, [ pos$1071 + h$1072.length, param$1233[1] ]);
                    }
                  }
                  return 0;
                });
           _(fill$1070, [ 0, al$1064 ]);
           return res$1069;
         });
    var concat$1075 =
      _f(function (al$1076) {
           var find_init$1077 =
             _f(function (aa$1078) {
                  if (aa$1078) {
                    {
                      var a$1079 = aa$1078[0];
                      if (a$1079.length > 0) return __(concat_aux$1062, [ a$1079[0], aa$1078 ]);
                      return __(find_init$1077, [ aa$1078[1] ]);
                    }
                  }
                  return $();
                });
           return __(find_init$1077, [ al$1076 ]);
         });
    var sub$1081 =
      _f(function (a$1082, ofs$1083, len$1084) {
           if (ofs$1083 < 0 || (len$1084 < 0 || ofs$1083 > a$1082.length - len$1084))
             return __(oc$Pervasives$[0], [ "Array.sub" ]);
           if (len$1084 === 0) return $();
           var r$1085 = caml_make_vect(len$1084, a$1082[ofs$1083]);
           for (var i$1086 = 1; i$1086 <= len$1084 - 1; i$1086++) {
             (function (i$1086) { r$1085[i$1086] = a$1082[ofs$1083 + i$1086]; }(i$1086));
           }
           return r$1085;
         });
    var fill$1087 =
      _f(function (a$1088, ofs$1089, len$1090, v$1091) {
           if (ofs$1089 < 0 || (len$1090 < 0 || ofs$1089 > a$1088.length - len$1090))
             return __(oc$Pervasives$[0], [ "Array.fill" ]);
           for (var i$1092 = ofs$1089; i$1092 <= ofs$1089 + len$1090 - 1; i$1092++) {
             (function (i$1092) { a$1088[i$1092] = v$1091; }(i$1092));
           }
         });
    var blit$1093 =
      _f(function (a1$1094, ofs1$1095, a2$1096, ofs2$1097, len$1098) {
           if (len$1098 < 0 ||
                 (ofs1$1095 < 0 ||
                    (ofs1$1095 > a1$1094.length - len$1098 || (ofs2$1097 < 0 || ofs2$1097 > a2$1096.length - len$1098))))
             return __(oc$Pervasives$[0], [ "Array.blit" ]);
           if (ofs1$1095 < ofs2$1097)
             for (var i$1099 = len$1098 - 1; i$1099 >= 0; i$1099--) {
               (function (i$1099) { a2$1096[ofs2$1097 + i$1099] = a1$1094[ofs1$1095 + i$1099]; }(i$1099));
             }
           for (var i$1100 = 0; i$1100 <= len$1098 - 1; i$1100++) {
             (function (i$1100) { a2$1096[ofs2$1097 + i$1100] = a1$1094[ofs1$1095 + i$1100]; }(i$1100));
           }
         });
    var iter$1101 =
      _f(function (f$1102, a$1103) {
           for (var i$1104 = 0; i$1104 <= a$1103.length - 1; i$1104++) {
             (function (i$1104) { _(f$1102, [ a$1103[i$1104] ]); }(i$1104));
           }
         });
    var map$1105 =
      _f(function (f$1106, a$1107) {
           var l$1108 = a$1107.length;
           if (l$1108 === 0) return $();
           var r$1109 = caml_make_vect(l$1108, _(f$1106, [ a$1107[0] ]));
           for (var i$1110 = 1; i$1110 <= l$1108 - 1; i$1110++) {
             (function (i$1110) { r$1109[i$1110] = _(f$1106, [ a$1107[i$1110] ]); }(i$1110));
           }
           return r$1109;
         });
    var iteri$1111 =
      _f(function (f$1112, a$1113) {
           for (var i$1114 = 0; i$1114 <= a$1113.length - 1; i$1114++) {
             (function (i$1114) { _(f$1112, [ i$1114, a$1113[i$1114] ]); }(i$1114));
           }
         });
    var mapi$1115 =
      _f(function (f$1116, a$1117) {
           var l$1118 = a$1117.length;
           if (l$1118 === 0) return $();
           var r$1119 = caml_make_vect(l$1118, _(f$1116, [ 0, a$1117[0] ]));
           for (var i$1120 = 1; i$1120 <= l$1118 - 1; i$1120++) {
             (function (i$1120) { r$1119[i$1120] = _(f$1116, [ i$1120, a$1117[i$1120] ]); }(i$1120));
           }
           return r$1119;
         });
    var to_list$1121 =
      _f(function (a$1122) {
           var tolist$1123 =
             _f(function (i$1124, res$1125) {
                  if (i$1124 < 0) return res$1125;
                  return __(tolist$1123, [ i$1124 - 1, $(a$1122[i$1124], res$1125) ]);
                });
           return __(tolist$1123, [ a$1122.length - 1, 0 ]);
         });
    var list_length$1126 =
      _f(function (accu$1127, param$1232) {
           if (param$1232) return __(list_length$1126, [ 1 + accu$1127, param$1232[1] ]);
           return accu$1127;
         });
    var of_list$1130 =
      _f(function (l$1133) {
           if (l$1133) {
             {
               var a$1134 = caml_make_vect(_(list_length$1126, [ 0, l$1133 ]), l$1133[0]);
               var fill$1135 =
                 _f(function (i$1136, param$1231) {
                      if (param$1231) { { a$1134[i$1136] = param$1231[0]; return __(fill$1135, [ i$1136 + 1, param$1231[1] ]); } }
                      return a$1134;
                    });
               return __(fill$1135, [ 1, l$1133[1] ]);
             }
           }
           return $();
         });
    var fold_left$1139 =
      _f(function (f$1140, x$1141, a$1142) {
           var r$1143 = x$1141;
           for (var i$1144 = 0; i$1144 <= a$1142.length - 1; i$1144++) {
             (function (i$1144) { r$1143 = _(f$1140, [ r$1143, a$1142[i$1144] ]); }(i$1144));
           }
           return r$1143;
         });
    var fold_right$1145 =
      _f(function (f$1146, a$1147, x$1148) {
           var r$1149 = x$1148;
           for (var i$1150 = a$1147.length - 1; i$1150 >= 0; i$1150--) {
             (function (i$1150) { r$1149 = _(f$1146, [ a$1147[i$1150], r$1149 ]); }(i$1150));
           }
           return r$1149;
         });
    var Bottom$1151 = $("Array.Bottom");
    var sort$1152 =
      _f(function (cmp$1153, a$1154) {
           var maxson$1155 =
             _f(function (l$1156, i$1157) {
                  var i31$1158 = i$1157 + i$1157 + i$1157 + 1;
                  var x$1159 = i31$1158;
                  if (i31$1158 + 2 < l$1156) {
                    {
                      if (_(cmp$1153, [ oc$$arefs(a$1154, i31$1158), oc$$arefs(a$1154, i31$1158 + 1) ]) < 0)
                        x$1159 = i31$1158 + 1;
                      else;
                      if (_(cmp$1153, [ oc$$arefs(a$1154, x$1159), oc$$arefs(a$1154, i31$1158 + 2) ]) < 0)
                        x$1159 = i31$1158 + 2;
                      else;
                      return x$1159;
                    }
                  }
                  if (i31$1158 + 1 < l$1156 && _(cmp$1153, [ oc$$arefs(a$1154, i31$1158), oc$$arefs(a$1154, i31$1158 + 1) ]) < 0)
                    return i31$1158 + 1;
                  if (i31$1158 < l$1156) return i31$1158;
                  throw $(Bottom$1151, i$1157);
                });
           var trickledown$1160 =
             _f(function (l$1161, i$1162, e$1163) {
                  var j$1164 = _(maxson$1155, [ l$1161, i$1162 ]);
                  if (_(cmp$1153, [ oc$$arefs(a$1154, j$1164), e$1163 ]) > 0) {
                    {
                      oc$$asets(a$1154, i$1162, oc$$arefs(a$1154, j$1164));
                      return __(trickledown$1160, [ l$1161, j$1164, e$1163 ]);
                    }
                  }
                  return oc$$asets(a$1154, i$1162, e$1163);
                });
           var trickle$1165 =
             _f(function (l$1166, i$1167, e$1168) {
                  try {
                    return _(trickledown$1160, [ l$1166, i$1167, e$1168 ]);
                  }
                  catch (exn$1230) {
                    if (exn$1230[0] === Bottom$1151) return oc$$asets(a$1154, exn$1230[1], e$1168);
                    throw exn$1230;
                  }
                });
           var bubbledown$1170 =
             _f(function (l$1171, i$1172) {
                  var j$1173 = _(maxson$1155, [ l$1171, i$1172 ]);
                  oc$$asets(a$1154, i$1172, oc$$arefs(a$1154, j$1173));
                  return __(bubbledown$1170, [ l$1171, j$1173 ]);
                });
           var bubble$1174 =
             _f(function (l$1175, i$1176) {
                  try {
                    return _(bubbledown$1170, [ l$1175, i$1176 ]);
                  }
                  catch (exn$1229) {
                    if (exn$1229[0] === Bottom$1151) return exn$1229[1];
                    throw exn$1229;
                  }
                });
           var trickleup$1178 =
             _f(function (i$1179, e$1180) {
                  var father$1181 = (i$1179 - 1) / 3 >> 0;
                  if (i$1179 !== father$1181); else throw $(Assert_failure$26g, $("ocaml/stdlib/array.ml", 209, 4));
                  if (_(cmp$1153, [ oc$$arefs(a$1154, father$1181), e$1180 ]) < 0) {
                    {
                      oc$$asets(a$1154, i$1179, oc$$arefs(a$1154, father$1181));
                      if (father$1181 > 0) return __(trickleup$1178, [ father$1181, e$1180 ]);
                      return oc$$asets(a$1154, 0, e$1180);
                    }
                  }
                  return oc$$asets(a$1154, i$1179, e$1180);
                });
           var l$1182 = a$1154.length;
           for (var i$1183 = ((l$1182 + 1) / 3 >> 0) - 1; i$1183 >= 0; i$1183--) {
             (function (i$1183) { _(trickle$1165, [ l$1182, i$1183, oc$$arefs(a$1154, i$1183) ]); }(i$1183));
           }
           for (var i$1184 = l$1182 - 1; i$1184 >= 2; i$1184--) {
             (function (i$1184) {
                var e$1185 = oc$$arefs(a$1154, i$1184);
                oc$$asets(a$1154, i$1184, oc$$arefs(a$1154, 0));
                _(trickleup$1178, [ _(bubble$1174, [ i$1184, 0 ]), e$1185 ]);
              }(i$1184));
           }
           if (l$1182 > 1) {
             {
               var e$1186 = oc$$arefs(a$1154, 1);
               oc$$asets(a$1154, 1, oc$$arefs(a$1154, 0));
               return oc$$asets(a$1154, 0, e$1186);
             }
           }
           return 0;
         });
    var cutoff$1187 = 5;
    var stable_sort$1188 =
      _f(function (cmp$1189, a$1190) {
           var merge$1191 =
             _f(function (src1ofs$1192, src1len$1193, src2$1194, src2ofs$1195, src2len$1196, dst$1197, dstofs$1198) {
                  var src1r$1199 = src1ofs$1192 + src1len$1193;
                  var src2r$1200 = src2ofs$1195 + src2len$1196;
                  var loop$1201 =
                    _f(function (i1$1202, s1$1203, i2$1204, s2$1205, d$1206) {
                         if (_(cmp$1189, [ s1$1203, s2$1205 ]) <= 0) {
                           {
                             oc$$asets(dst$1197, d$1206, s1$1203);
                             var i1$1207 = i1$1202 + 1;
                             if (i1$1207 < src1r$1199)
                               return __(loop$1201, [ i1$1207, oc$$arefs(a$1190, i1$1207), i2$1204, s2$1205, d$1206 + 1 ]);
                             return __(blit$1093, [ src2$1194, i2$1204, dst$1197, d$1206 + 1, src2r$1200 - i2$1204 ]);
                           }
                         }
                         oc$$asets(dst$1197, d$1206, s2$1205);
                         var i2$1208 = i2$1204 + 1;
                         if (i2$1208 < src2r$1200)
                           return __(loop$1201, [ i1$1202, s1$1203, i2$1208, oc$$arefs(src2$1194, i2$1208), d$1206 + 1 ]);
                         return __(blit$1093, [ a$1190, i1$1202, dst$1197, d$1206 + 1, src1r$1199 - i1$1202 ]);
                       });
                  return __(loop$1201,
                            [
                              src1ofs$1192,
                              oc$$arefs(a$1190, src1ofs$1192),
                              src2ofs$1195,
                              oc$$arefs(src2$1194, src2ofs$1195),
                              dstofs$1198
                            ]);
                });
           var isortto$1209 =
             _f(function (srcofs$1210, dst$1211, dstofs$1212, len$1213) {
                  for (var i$1214 = 0; i$1214 <= len$1213 - 1; i$1214++) {
                    (function (i$1214) {
                       var e$1215 = oc$$arefs(a$1190, srcofs$1210 + i$1214);
                       var j$1216 = dstofs$1212 + i$1214 - 1;
                       while (j$1216 >= dstofs$1212 && _(cmp$1189, [ oc$$arefs(dst$1211, j$1216), e$1215 ]) > 0) {
                         { oc$$asets(dst$1211, j$1216 + 1, oc$$arefs(dst$1211, j$1216)); j$1216 = -1 + j$1216; }
                       }
                       oc$$asets(dst$1211, j$1216 + 1, e$1215);
                     }(i$1214));
                  }
                });
           var sortto$1217 =
             _f(function (srcofs$1218, dst$1219, dstofs$1220, len$1221) {
                  if (len$1221 <= cutoff$1187) return __(isortto$1209, [ srcofs$1218, dst$1219, dstofs$1220, len$1221 ]);
                  var l1$1222 = len$1221 / 2 >> 0;
                  var l2$1223 = len$1221 - l1$1222;
                  _(sortto$1217, [ srcofs$1218 + l1$1222, dst$1219, dstofs$1220 + l1$1222, l2$1223 ]);
                  _(sortto$1217, [ srcofs$1218, a$1190, srcofs$1218 + l2$1223, l1$1222 ]);
                  return __(merge$1191,
                            [ srcofs$1218 + l2$1223, l1$1222, dst$1219, dstofs$1220 + l1$1222, l2$1223, dst$1219, dstofs$1220 ]);
                });
           var l$1224 = a$1190.length;
           if (l$1224 <= cutoff$1187) return __(isortto$1209, [ 0, a$1190, 0, l$1224 ]);
           var l1$1225 = l$1224 / 2 >> 0;
           var l2$1226 = l$1224 - l1$1225;
           var t$1227 = caml_make_vect(l2$1226, oc$$arefs(a$1190, 0));
           _(sortto$1217, [ l1$1225, t$1227, 0, l2$1226 ]);
           _(sortto$1217, [ 0, a$1190, l2$1226, l1$1225 ]);
           return __(merge$1191, [ l2$1226, l1$1225, t$1227, 0, l2$1226, a$1190, 0 ]);
         });
    return $(init$1037, make_matrix$1042, make_matrix$1042, append$1054, concat$1075, sub$1081, copy$1049, fill$1087, blit$1093,
             to_list$1121, of_list$1130, iter$1101, map$1105, iteri$1111, mapi$1115, fold_left$1139, fold_right$1145, sort$1152,
             stable_sort$1188, stable_sort$1188);
  }();
var oc$List$ =
  function () {
    var length_aux$1030 =
      _f(function (len$1031, param$1366) {
           if (param$1366) return __(length_aux$1030, [ len$1031 + 1, param$1366[1] ]);
           return len$1031;
         });
    var length$1034 = _f(function (l$1035) { return __(length_aux$1030, [ 0, l$1035 ]); });
    var hd$1036 = _f(function (param$1365) { if (param$1365) return param$1365[0]; return __(oc$Pervasives$[1], [ "hd" ]); });
    var tl$1039 = _f(function (param$1364) { if (param$1364) return param$1364[1]; return __(oc$Pervasives$[1], [ "tl" ]); });
    var nth$1042 =
      _f(function (l$1043, n$1044) {
           if (n$1044 < 0) return __(oc$Pervasives$[0], [ "List.nth" ]);
           var nth_aux$1045 =
             _f(function (l$1046, n$1047) {
                  if (!l$1046) return __(oc$Pervasives$[1], [ "nth" ]);
                  if (n$1047 === 0) return l$1046[0];
                  return __(nth_aux$1045, [ l$1046[1], n$1047 - 1 ]);
                });
           return __(nth_aux$1045, [ l$1043, n$1044 ]);
         });
    var append$1050 = oc$Pervasives$[21];
    var rev_append$1051 =
      _f(function (l1$1052, l2$1053) {
           if (l1$1052) return __(rev_append$1051, [ l1$1052[1], $(l1$1052[0], l2$1053) ]);
           return l2$1053;
         });
    var rev$1056 = _f(function (l$1057) { return __(rev_append$1051, [ l$1057, 0 ]); });
    var flatten$1058 =
      _f(function (param$1363) {
           if (param$1363) return __(oc$Pervasives$[21], [ param$1363[0], _(flatten$1058, [ param$1363[1] ]) ]);
           return 0;
         });
    var map$1062 =
      _f(function (f$1063, param$1362) {
           if (param$1362) {
             { var r$1066 = _(f$1063, [ param$1362[0] ]); return $(r$1066, _(map$1062, [ f$1063, param$1362[1] ])); }
           }
           return 0;
         });
    var rev_map$1067 =
      _f(function (f$1068, l$1069) {
           var rmap_f$1070 =
             _f(function (accu$1071, param$1361) {
                  if (param$1361) return __(rmap_f$1070, [ $(_(f$1068, [ param$1361[0] ]), accu$1071), param$1361[1] ]);
                  return accu$1071;
                });
           return __(rmap_f$1070, [ 0, l$1069 ]);
         });
    var iter$1074 =
      _f(function (f$1075, param$1360) {
           if (param$1360) { { _(f$1075, [ param$1360[0] ]); return __(iter$1074, [ f$1075, param$1360[1] ]); } }
           return 0;
         });
    var fold_left$1078 =
      _f(function (f$1079, accu$1080, l$1081) {
           if (l$1081) return __(fold_left$1078, [ f$1079, _(f$1079, [ accu$1080, l$1081[0] ]), l$1081[1] ]);
           return accu$1080;
         });
    var fold_right$1084 =
      _f(function (f$1085, l$1086, accu$1087) {
           if (l$1086) return __(f$1085, [ l$1086[0], _(fold_right$1084, [ f$1085, l$1086[1], accu$1087 ]) ]);
           return accu$1087;
         });
    var map2$1090 =
      _f(function (f$1091, l1$1092, l2$1093) {
           var $r34 = false;
           r$34: {
             {
               if (!l1$1092) { { if (l2$1093) { { $r34 = true; break r$34; } } return 0; } }
               if (!l2$1093) { { $r34 = true; break r$34; } }
               var r$1098 = _(f$1091, [ l1$1092[0], l2$1093[0] ]);
               return $(r$1098, _(map2$1090, [ f$1091, l1$1092[1], l2$1093[1] ]));
             }
           }
           if ($r34) return __(oc$Pervasives$[0], [ "List.map2" ]);
         });
    var rev_map2$1099 =
      _f(function (f$1100, l1$1101, l2$1102) {
           var rmap2_f$1103 =
             _f(function (accu$1104, l1$1105, l2$1106) {
                  var $r31 = false;
                  r$31: {
                    {
                      if (!l1$1105) { { if (l2$1106) { { $r31 = true; break r$31; } } return accu$1104; } }
                      if (!l2$1106) { { $r31 = true; break r$31; } }
                      return __(rmap2_f$1103, [ $(_(f$1100, [ l1$1105[0], l2$1106[0] ]), accu$1104), l1$1105[1], l2$1106[1] ]);
                    }
                  }
                  if ($r31) return __(oc$Pervasives$[0], [ "List.rev_map2" ]);
                });
           return __(rmap2_f$1103, [ 0, l1$1101, l2$1102 ]);
         });
    var iter2$1111 =
      _f(function (f$1112, l1$1113, l2$1114) {
           var $r30 = false;
           r$30: {
             {
               if (!l1$1113) { { if (l2$1114) { { $r30 = true; break r$30; } } return 0; } }
               if (!l2$1114) { { $r30 = true; break r$30; } }
               _(f$1112, [ l1$1113[0], l2$1114[0] ]);
               return __(iter2$1111, [ f$1112, l1$1113[1], l2$1114[1] ]);
             }
           }
           if ($r30) return __(oc$Pervasives$[0], [ "List.iter2" ]);
         });
    var fold_left2$1119 =
      _f(function (f$1120, accu$1121, l1$1122, l2$1123) {
           var $r29 = false;
           r$29: {
             {
               if (!l1$1122) { { if (l2$1123) { { $r29 = true; break r$29; } } return accu$1121; } }
               if (!l2$1123) { { $r29 = true; break r$29; } }
               return __(fold_left2$1119, [ f$1120, _(f$1120, [ accu$1121, l1$1122[0], l2$1123[0] ]), l1$1122[1], l2$1123[1] ]);
             }
           }
           if ($r29) return __(oc$Pervasives$[0], [ "List.fold_left2" ]);
         });
    var fold_right2$1128 =
      _f(function (f$1129, l1$1130, l2$1131, accu$1132) {
           var $r28 = false;
           r$28: {
             {
               if (!l1$1130) { { if (l2$1131) { { $r28 = true; break r$28; } } return accu$1132; } }
               if (!l2$1131) { { $r28 = true; break r$28; } }
               return __(f$1129, [ l1$1130[0], l2$1131[0], _(fold_right2$1128, [ f$1129, l1$1130[1], l2$1131[1], accu$1132 ]) ]);
             }
           }
           if ($r28) return __(oc$Pervasives$[0], [ "List.fold_right2" ]);
         });
    var for_all$1137 =
      _f(function (p$1138, param$1349) {
           if (param$1349) return _(p$1138, [ param$1349[0] ]) && _(for_all$1137, [ p$1138, param$1349[1] ]);
           return true;
         });
    var exists$1141 =
      _f(function (p$1142, param$1348) {
           if (param$1348) return _(p$1142, [ param$1348[0] ]) || _(exists$1141, [ p$1142, param$1348[1] ]);
           return false;
         });
    var for_all2$1145 =
      _f(function (p$1146, l1$1147, l2$1148) {
           var $r27 = false;
           r$27: {
             {
               if (!l1$1147) { { if (l2$1148) { { $r27 = true; break r$27; } } return true; } }
               if (!l2$1148) { { $r27 = true; break r$27; } }
               return _(p$1146, [ l1$1147[0], l2$1148[0] ]) && _(for_all2$1145, [ p$1146, l1$1147[1], l2$1148[1] ]);
             }
           }
           if ($r27) return __(oc$Pervasives$[0], [ "List.for_all2" ]);
         });
    var exists2$1153 =
      _f(function (p$1154, l1$1155, l2$1156) {
           var $r26 = false;
           r$26: {
             {
               if (!l1$1155) { { if (l2$1156) { { $r26 = true; break r$26; } } return false; } }
               if (!l2$1156) { { $r26 = true; break r$26; } }
               return _(p$1154, [ l1$1155[0], l2$1156[0] ]) || _(exists2$1153, [ p$1154, l1$1155[1], l2$1156[1] ]);
             }
           }
           if ($r26) return __(oc$Pervasives$[0], [ "List.exists2" ]);
         });
    var mem$1161 =
      _f(function (x$1162, param$1343) {
           if (param$1343) return caml_compare(param$1343[0], x$1162) === 0 || _(mem$1161, [ x$1162, param$1343[1] ]);
           return false;
         });
    var memq$1165 =
      _f(function (x$1166, param$1342) {
           if (param$1342) return param$1342[0] === x$1166 || _(memq$1165, [ x$1166, param$1342[1] ]);
           return false;
         });
    var assoc$1169 =
      _f(function (x$1170, param$1340) {
           if (param$1340) {
             {
               var match$1341 = param$1340[0];
               if (caml_compare(match$1341[0], x$1170) === 0) return match$1341[1];
               return __(assoc$1169, [ x$1170, param$1340[1] ]);
             }
           }
           throw $(Not_found$20g);
         });
    var assq$1174 =
      _f(function (x$1175, param$1338) {
           if (param$1338) {
             {
               var match$1339 = param$1338[0];
               if (match$1339[0] === x$1175) return match$1339[1];
               return __(assq$1174, [ x$1175, param$1338[1] ]);
             }
           }
           throw $(Not_found$20g);
         });
    var mem_assoc$1179 =
      _f(function (x$1180, param$1336) {
           if (param$1336) return caml_compare(param$1336[0][0], x$1180) === 0 || _(mem_assoc$1179, [ x$1180, param$1336[1] ]);
           return false;
         });
    var mem_assq$1184 =
      _f(function (x$1185, param$1334) {
           if (param$1334) return param$1334[0][0] === x$1185 || _(mem_assq$1184, [ x$1185, param$1334[1] ]);
           return false;
         });
    var remove_assoc$1189 =
      _f(function (x$1190, param$1333) {
           if (param$1333) {
             {
               var l$1194 = param$1333[1];
               var pair$1193 = param$1333[0];
               if (caml_compare(pair$1193[0], x$1190) === 0) return l$1194;
               return $(pair$1193, _(remove_assoc$1189, [ x$1190, l$1194 ]));
             }
           }
           return 0;
         });
    var remove_assq$1195 =
      _f(function (x$1196, param$1332) {
           if (param$1332) {
             {
               var l$1200 = param$1332[1];
               var pair$1199 = param$1332[0];
               if (pair$1199[0] === x$1196) return l$1200;
               return $(pair$1199, _(remove_assq$1195, [ x$1196, l$1200 ]));
             }
           }
           return 0;
         });
    var find$1201 =
      _f(function (p$1202, param$1331) {
           if (param$1331) {
             {
               var x$1203 = param$1331[0];
               if (_(p$1202, [ x$1203 ])) return x$1203;
               return __(find$1201, [ p$1202, param$1331[1] ]);
             }
           }
           throw $(Not_found$20g);
         });
    var find_all$1205 =
      _f(function (p$1206) {
           var find$1207 =
             _f(function (accu$1208, param$1330) {
                  if (param$1330) {
                    {
                      var l$1210 = param$1330[1];
                      var x$1209 = param$1330[0];
                      if (_(p$1206, [ x$1209 ])) return __(find$1207, [ $(x$1209, accu$1208), l$1210 ]);
                      return __(find$1207, [ accu$1208, l$1210 ]);
                    }
                  }
                  return __(rev$1056, [ accu$1208 ]);
                });
           return __(find$1207, [ 0 ]);
         });
    var partition$1212 =
      _f(function (p$1213, l$1214) {
           var part$1215 =
             _f(function (yes$1216, no$1217, param$1329) {
                  if (param$1329) {
                    {
                      var l$1219 = param$1329[1];
                      var x$1218 = param$1329[0];
                      if (_(p$1213, [ x$1218 ])) return __(part$1215, [ $(x$1218, yes$1216), no$1217, l$1219 ]);
                      return __(part$1215, [ yes$1216, $(x$1218, no$1217), l$1219 ]);
                    }
                  }
                  return $(_(rev$1056, [ yes$1216 ]), _(rev$1056, [ no$1217 ]));
                });
           return __(part$1215, [ 0, 0, l$1214 ]);
         });
    var split$1220 =
      _f(function (param$1326) {
           if (param$1326) {
             {
               var match$1328 = param$1326[0];
               var match$1327 = _(split$1220, [ param$1326[1] ]);
               return $($(match$1328[0], match$1327[0]), $(match$1328[1], match$1327[1]));
             }
           }
           return $(0, 0);
         });
    var combine$1226 =
      _f(function (l1$1227, l2$1228) {
           var $r21 = false;
           r$21: {
             {
               if (!l1$1227) { { if (l2$1228) { { $r21 = true; break r$21; } } return 0; } }
               if (!l2$1228) { { $r21 = true; break r$21; } }
               return $($(l1$1227[0], l2$1228[0]), _(combine$1226, [ l1$1227[1], l2$1228[1] ]));
             }
           }
           if ($r21) return __(oc$Pervasives$[0], [ "List.combine" ]);
         });
    var merge$1233 =
      _f(function (cmp$1234, l1$1235, l2$1236) {
           if (!l1$1235) return l2$1236;
           if (l2$1236) {
             {
               var h2$1241 = l2$1236[0];
               var h1$1239 = l1$1235[0];
               if (_(cmp$1234, [ h1$1239, h2$1241 ]) <= 0) return $(h1$1239, _(merge$1233, [ cmp$1234, l1$1235[1], l2$1236 ]));
               return $(h2$1241, _(merge$1233, [ cmp$1234, l1$1235, l2$1236[1] ]));
             }
           }
           return l1$1235;
         });
    var chop$1243 =
      _f(function (k$1244, l$1245) {
           if (k$1244 === 0) return l$1245;
           if (l$1245) return __(chop$1243, [ k$1244 - 1, l$1245[1] ]);
           throw $(Assert_failure$26g, $("ocaml/stdlib/list.ml", 213, 11));
         });
    var stable_sort$1248 =
      _f(function (cmp$1249, l$1250) {
           var rev_merge$1251 =
             _f(function (l1$1252, l2$1253, accu$1254) {
                  if (!l1$1252) return __(rev_append$1051, [ l2$1253, accu$1254 ]);
                  if (l2$1253) {
                    {
                      var h2$1259 = l2$1253[0];
                      var h1$1257 = l1$1252[0];
                      if (_(cmp$1249, [ h1$1257, h2$1259 ]) <= 0)
                        return __(rev_merge$1251, [ l1$1252[1], l2$1253, $(h1$1257, accu$1254) ]);
                      return __(rev_merge$1251, [ l1$1252, l2$1253[1], $(h2$1259, accu$1254) ]);
                    }
                  }
                  return __(rev_append$1051, [ l1$1252, accu$1254 ]);
                });
           var rev_merge_rev$1261 =
             _f(function (l1$1262, l2$1263, accu$1264) {
                  if (!l1$1262) return __(rev_append$1051, [ l2$1263, accu$1264 ]);
                  if (l2$1263) {
                    {
                      var h2$1269 = l2$1263[0];
                      var h1$1267 = l1$1262[0];
                      if (_(cmp$1249, [ h1$1267, h2$1269 ]) > 0)
                        return __(rev_merge_rev$1261, [ l1$1262[1], l2$1263, $(h1$1267, accu$1264) ]);
                      return __(rev_merge_rev$1261, [ l1$1262, l2$1263[1], $(h2$1269, accu$1264) ]);
                    }
                  }
                  return __(rev_append$1051, [ l1$1262, accu$1264 ]);
                });
           var sort$1271 =
             _f(function (n$1273, l$1274) {
                  var $r9 = false;
                  r$9: {
                    {
                      if (!(n$1273 !== 2)) {
                        {
                          if (!l$1274) { { $r9 = true; break r$9; } }
                          var match$1306 = l$1274[1];
                          if (!match$1306) { { $r9 = true; break r$9; } }
                          var x2$1276 = match$1306[0];
                          var x1$1275 = l$1274[0];
                          if (_(cmp$1249, [ x1$1275, x2$1276 ]) <= 0) return $(x1$1275, $(x2$1276, 0));
                          return $(x2$1276, $(x1$1275, 0));
                        }
                      }
                      if (n$1273 !== 3) { { $r9 = true; break r$9; } }
                      if (!l$1274) { { $r9 = true; break r$9; } }
                      var match$1308 = l$1274[1];
                      if (!match$1308) { { $r9 = true; break r$9; } }
                      var match$1309 = match$1308[1];
                      if (!match$1309) { { $r9 = true; break r$9; } }
                      var x3$1279 = match$1309[0];
                      var x2$1278 = match$1308[0];
                      var x1$1277 = l$1274[0];
                      if (!(_(cmp$1249, [ x1$1277, x2$1278 ]) <= 0)) {
                        {
                          if (_(cmp$1249, [ x1$1277, x3$1279 ]) <= 0) return $(x2$1278, $(x1$1277, $(x3$1279, 0)));
                          if (_(cmp$1249, [ x2$1278, x3$1279 ]) <= 0) return $(x2$1278, $(x3$1279, $(x1$1277, 0)));
                          return $(x3$1279, $(x2$1278, $(x1$1277, 0)));
                        }
                      }
                      if (_(cmp$1249, [ x2$1278, x3$1279 ]) <= 0) return $(x1$1277, $(x2$1278, $(x3$1279, 0)));
                      if (_(cmp$1249, [ x1$1277, x3$1279 ]) <= 0) return $(x1$1277, $(x3$1279, $(x2$1278, 0)));
                      return $(x3$1279, $(x1$1277, $(x2$1278, 0)));
                    }
                  }
                  if ($r9) {
                    {
                      var n1$1282 = n$1273 >>> 1;
                      var n2$1283 = n$1273 - n1$1282;
                      var l2$1284 = _(chop$1243, [ n1$1282, l$1274 ]);
                      var s1$1285 = _(rev_sort$1272, [ n1$1282, l$1274 ]);
                      var s2$1286 = _(rev_sort$1272, [ n2$1283, l2$1284 ]);
                      return __(rev_merge_rev$1261, [ s1$1285, s2$1286, 0 ]);
                    }
                  }
                });
           var rev_sort$1272 =
             _f(function (n$1287, l$1288) {
                  var $r15 = false;
                  r$15: {
                    {
                      if (!(n$1287 !== 2)) {
                        {
                          if (!l$1288) { { $r15 = true; break r$15; } }
                          var match$1313 = l$1288[1];
                          if (!match$1313) { { $r15 = true; break r$15; } }
                          var x2$1290 = match$1313[0];
                          var x1$1289 = l$1288[0];
                          if (_(cmp$1249, [ x1$1289, x2$1290 ]) > 0) return $(x1$1289, $(x2$1290, 0));
                          return $(x2$1290, $(x1$1289, 0));
                        }
                      }
                      if (n$1287 !== 3) { { $r15 = true; break r$15; } }
                      if (!l$1288) { { $r15 = true; break r$15; } }
                      var match$1315 = l$1288[1];
                      if (!match$1315) { { $r15 = true; break r$15; } }
                      var match$1316 = match$1315[1];
                      if (!match$1316) { { $r15 = true; break r$15; } }
                      var x3$1293 = match$1316[0];
                      var x2$1292 = match$1315[0];
                      var x1$1291 = l$1288[0];
                      if (!(_(cmp$1249, [ x1$1291, x2$1292 ]) > 0)) {
                        {
                          if (_(cmp$1249, [ x1$1291, x3$1293 ]) > 0) return $(x2$1292, $(x1$1291, $(x3$1293, 0)));
                          if (_(cmp$1249, [ x2$1292, x3$1293 ]) > 0) return $(x2$1292, $(x3$1293, $(x1$1291, 0)));
                          return $(x3$1293, $(x2$1292, $(x1$1291, 0)));
                        }
                      }
                      if (_(cmp$1249, [ x2$1292, x3$1293 ]) > 0) return $(x1$1291, $(x2$1292, $(x3$1293, 0)));
                      if (_(cmp$1249, [ x1$1291, x3$1293 ]) > 0) return $(x1$1291, $(x3$1293, $(x2$1292, 0)));
                      return $(x3$1293, $(x1$1291, $(x2$1292, 0)));
                    }
                  }
                  if ($r15) {
                    {
                      var n1$1296 = n$1287 >>> 1;
                      var n2$1297 = n$1287 - n1$1296;
                      var l2$1298 = _(chop$1243, [ n1$1296, l$1288 ]);
                      var s1$1299 = _(sort$1271, [ n1$1296, l$1288 ]);
                      var s2$1300 = _(sort$1271, [ n2$1297, l2$1298 ]);
                      return __(rev_merge$1251, [ s1$1299, s2$1300, 0 ]);
                    }
                  }
                });
           var len$1301 = _(length$1034, [ l$1250 ]);
           if (len$1301 < 2) return l$1250;
           return __(sort$1271, [ len$1301, l$1250 ]);
         });
    return $(length$1034, hd$1036, tl$1039, nth$1042, rev$1056, append$1050, rev_append$1051, flatten$1058, flatten$1058,
             iter$1074, map$1062, rev_map$1067, fold_left$1078, fold_right$1084, iter2$1111, map2$1090, rev_map2$1099,
             fold_left2$1119, fold_right2$1128, for_all$1137, exists$1141, for_all2$1145, exists2$1153, mem$1161, memq$1165,
             find$1201, find_all$1205, find_all$1205, partition$1212, assoc$1169, assq$1174, mem_assoc$1179, mem_assq$1184,
             remove_assoc$1189, remove_assq$1195, split$1220, combine$1226, stable_sort$1248, stable_sort$1248, stable_sort$1248,
             merge$1233);
  }();
var oc$Char$ =
  function () {
    var chr$1032 =
      _f(function (n$1033) { if (n$1033 < 0 || n$1033 > 255) return __(oc$Pervasives$[0], [ "Char.chr" ]); return n$1033; });
    var escaped$1038 =
      _f(function (c$1039) {
           var $r7 = false;
           r$7: {
             {
               if (!(c$1039 !== 39)) return "\\\'";
               if (!(c$1039 !== 92)) return "\\\\";
               if (c$1039 >= 14) { { $r7 = true; break r$7; } }
               switch (c$1039)
               {
               case 0: $r7 = true; break r$7;
               case 1: $r7 = true; break r$7;
               case 2: $r7 = true; break r$7;
               case 3: $r7 = true; break r$7;
               case 4: $r7 = true; break r$7;
               case 5: $r7 = true; break r$7;
               case 6: $r7 = true; break r$7;
               case 7: $r7 = true; break r$7;
               case 8: return "\\b";
               case 9: return "\\t";
               case 10: return "\\n";
               case 11: $r7 = true; break r$7;
               case 12: $r7 = true; break r$7;
               case 13: return "\\r";
               default: return null;
               }
             }
           }
           if ($r7) {
             {
               if (caml_is_printable(c$1039)) { { var s$1040 = oc$$cms(1); oc$$ssetu(s$1040, 0, c$1039); return s$1040; } }
               var n$1041 = c$1039;
               var s$1042 = oc$$cms(4);
               oc$$ssetu(s$1042, 0, 92);
               oc$$ssetu(s$1042, 1, 48 + (n$1041 / 100 >> 0));
               oc$$ssetu(s$1042, 2, 48 + (n$1041 / 10 >> 0) % 10);
               oc$$ssetu(s$1042, 3, 48 + n$1041 % 10);
               return s$1042;
             }
           }
         });
    var lowercase$1043 =
      _f(function (c$1044) {
           if (c$1044 >= 65 && c$1044 <= 90 || (c$1044 >= 192 && c$1044 <= 214 || c$1044 >= 216 && c$1044 <= 222))
             return c$1044 + 32;
           return c$1044;
         });
    var uppercase$1045 =
      _f(function (c$1046) {
           if (c$1046 >= 97 && c$1046 <= 122 || (c$1046 >= 224 && c$1046 <= 246 || c$1046 >= 248 && c$1046 <= 254))
             return c$1046 - 32;
           return c$1046;
         });
    var compare$1048 = _f(function (c1$1049, c2$1050) { return c1$1049 - c2$1050; });
    return $(chr$1032, escaped$1038, lowercase$1043, uppercase$1045, compare$1048);
  }();
var oc$String$ =
  function () {
    var make$1038 =
      _f(function (n$1039, c$1040) { var s$1041 = oc$$cms(n$1039); caml_fill_string(s$1041, 0, n$1039, c$1040); return s$1041; });
    var copy$1042 =
      _f(function (s$1043) {
           var len$1044 = s$1043.length;
           var r$1045 = oc$$cms(len$1044);
           caml_blit_string(s$1043, 0, r$1045, 0, len$1044);
           return r$1045;
         });
    var sub$1046 =
      _f(function (s$1047, ofs$1048, len$1049) {
           if (ofs$1048 < 0 || (len$1049 < 0 || ofs$1048 > s$1047.length - len$1049))
             return __(oc$Pervasives$[0], [ "String.sub" ]);
           var r$1050 = oc$$cms(len$1049);
           caml_blit_string(s$1047, ofs$1048, r$1050, 0, len$1049);
           return r$1050;
         });
    var fill$1051 =
      _f(function (s$1052, ofs$1053, len$1054, c$1055) {
           if (ofs$1053 < 0 || (len$1054 < 0 || ofs$1053 > s$1052.length - len$1054))
             return __(oc$Pervasives$[0], [ "String.fill" ]);
           return caml_fill_string(s$1052, ofs$1053, len$1054, c$1055);
         });
    var blit$1056 =
      _f(function (s1$1057, ofs1$1058, s2$1059, ofs2$1060, len$1061) {
           if (len$1061 < 0 ||
                 (ofs1$1058 < 0 ||
                    (ofs1$1058 > s1$1057.length - len$1061 || (ofs2$1060 < 0 || ofs2$1060 > s2$1059.length - len$1061))))
             return __(oc$Pervasives$[0], [ "String.blit" ]);
           return caml_blit_string(s1$1057, ofs1$1058, s2$1059, ofs2$1060, len$1061);
         });
    var iter$1062 =
      _f(function (f$1063, a$1064) {
           for (var i$1065 = 0; i$1065 <= a$1064.length - 1; i$1065++) {
             (function (i$1065) { _(f$1063, [ oc$$srefu(a$1064, i$1065) ]); }(i$1065));
           }
         });
    var concat$1066 =
      _f(function (sep$1067, l$1068) {
           if (l$1068) {
             {
               var hd$1069 = l$1068[0];
               var num$1071 = $(0);
               var len$1072 = $(0);
               _(oc$List$[9],
                 [ _f(function (s$1073) { num$1071[0]++; return len$1072[0] = len$1072[0] + s$1073.length; }), l$1068 ]);
               var r$1074 = oc$$cms(len$1072[0] + sep$1067.length * (num$1071[0] - 1));
               caml_blit_string(hd$1069, 0, r$1074, 0, hd$1069.length);
               var pos$1075 = $(hd$1069.length);
               _(oc$List$[9],
                 [
                   _f(function (s$1076) {
                        caml_blit_string(sep$1067, 0, r$1074, pos$1075[0], sep$1067.length);
                        pos$1075[0] = pos$1075[0] + sep$1067.length;
                        caml_blit_string(s$1076, 0, r$1074, pos$1075[0], s$1076.length);
                        return pos$1075[0] = pos$1075[0] + s$1076.length;
                      }),
                   l$1068[1]
                 ]);
               return r$1074;
             }
           }
           return "";
         });
    var escaped$1080 =
      _f(function (s$1081) {
           var n$1082 = 0;
           for (var i$1083 = 0; i$1083 <= s$1081.length - 1; i$1083++) {
             (function (i$1083) {
                n$1082 =
                  n$1082 +
                    function () {
                      var c$1084 = oc$$srefu(s$1081, i$1083);
                      var $r26 = false;
                      r$26: {
                        {
                          var $r27 = false;
                          r$27: {
                            {
                              if (!(c$1084 >= 14)) {
                                {
                                  if (!(c$1084 >= 11)) {
                                    { if (!(c$1084 >= 8)) { { $r27 = true; break r$27; } } $r26 = true; break r$26; }
                                  }
                                  if (!(c$1084 >= 13)) { { $r27 = true; break r$27; } }
                                  $r26 = true;
                                  break r$26;
                                }
                              }
                              if (!(c$1084 !== 34)) { { $r26 = true; break r$26; } }
                              if (!(c$1084 !== 92)) { { $r26 = true; break r$26; } }
                              $r27 = true;
                              break r$27;
                            }
                          }
                          if ($r27) { { if (caml_is_printable(c$1084)) return 1; return 4; } }
                        }
                      }
                      if ($r26) return 2;
                    }();
              }(i$1083));
           }
           if (n$1082 === s$1081.length) return s$1081;
           var s$27$1085 = oc$$cms(n$1082);
           n$1082 = 0;
           for (var i$1086 = 0; i$1086 <= s$1081.length - 1; i$1086++) {
             (function (i$1086) {
                var c$1087 = oc$$srefu(s$1081, i$1086);
                var $r24 = false;
                r$24: {
                  {
                    var switcher$1150 = -34 + c$1087;
                    if (!(switcher$1150 < 0 || switcher$1150 > 58)) {
                      {
                        if (!(-1 + switcher$1150 < 0 || -1 + switcher$1150 > 56)) { { $r24 = true; break r$24; } }
                        oc$$ssetu(s$27$1085, n$1082, 92);
                        n$1082 = 1 + n$1082;
                        oc$$ssetu(s$27$1085, n$1082, c$1087);
                      }
                    }
                    else {
                      {
                        if (switcher$1150 >= -20) { { $r24 = true; break r$24; } }
                        var s$1153 = 34 + switcher$1150;
                        switch (s$1153)
                        {
                        case 0: $r24 = true; break r$24;
                        case 1: $r24 = true; break r$24;
                        case 2: $r24 = true; break r$24;
                        case 3: $r24 = true; break r$24;
                        case 4: $r24 = true; break r$24;
                        case 5: $r24 = true; break r$24;
                        case 6: $r24 = true; break r$24;
                        case 7: $r24 = true; break r$24;
                        case 8: oc$$ssetu(s$27$1085, n$1082, 92); n$1082 = 1 + n$1082; oc$$ssetu(s$27$1085, n$1082, 98); break;
                        case 9: oc$$ssetu(s$27$1085, n$1082, 92); n$1082 = 1 + n$1082; oc$$ssetu(s$27$1085, n$1082, 116); break;
                        case 10: oc$$ssetu(s$27$1085, n$1082, 92); n$1082 = 1 + n$1082; oc$$ssetu(s$27$1085, n$1082, 110); break;
                        case 11: $r24 = true; break r$24;
                        case 12: $r24 = true; break r$24;
                        case 13: oc$$ssetu(s$27$1085, n$1082, 92); n$1082 = 1 + n$1082; oc$$ssetu(s$27$1085, n$1082, 114); break;
                        default: null;
                        }
                      }
                    }
                  }
                }
                if ($r24)
                  if (caml_is_printable(c$1087))
                    oc$$ssetu(s$27$1085, n$1082, c$1087);
                  else {
                    {
                      var a$1089 = c$1087;
                      oc$$ssetu(s$27$1085, n$1082, 92);
                      n$1082 = 1 + n$1082;
                      oc$$ssetu(s$27$1085, n$1082, 48 + (a$1089 / 100 >> 0));
                      n$1082 = 1 + n$1082;
                      oc$$ssetu(s$27$1085, n$1082, 48 + (a$1089 / 10 >> 0) % 10);
                      n$1082 = 1 + n$1082;
                      oc$$ssetu(s$27$1085, n$1082, 48 + a$1089 % 10);
                    }
                  }
                n$1082 = 1 + n$1082;
              }(i$1086));
           }
           return s$27$1085;
         });
    var map$1090 =
      _f(function (f$1091, s$1092) {
           var l$1093 = s$1092.length;
           if (l$1093 === 0) return s$1092;
           var r$1094 = oc$$cms(l$1093);
           for (var i$1095 = 0; i$1095 <= l$1093 - 1; i$1095++) {
             (function (i$1095) { oc$$ssetu(r$1094, i$1095, _(f$1091, [ oc$$srefu(s$1092, i$1095) ])); }(i$1095));
           }
           return r$1094;
         });
    var uppercase$1096 = _f(function (s$1097) { return __(map$1090, [ oc$Char$[3], s$1097 ]); });
    var lowercase$1098 = _f(function (s$1099) { return __(map$1090, [ oc$Char$[2], s$1099 ]); });
    var apply1$1100 =
      _f(function (f$1101, s$1102) {
           if (s$1102.length === 0) return s$1102;
           var r$1103 = _(copy$1042, [ s$1102 ]);
           oc$$ssetu(r$1103, 0, _(f$1101, [ oc$$srefu(s$1102, 0) ]));
           return r$1103;
         });
    var capitalize$1104 = _f(function (s$1105) { return __(apply1$1100, [ oc$Char$[3], s$1105 ]); });
    var uncapitalize$1106 = _f(function (s$1107) { return __(apply1$1100, [ oc$Char$[2], s$1107 ]); });
    var index_rec$1108 =
      _f(function (s$1109, lim$1110, i$1111, c$1112) {
           if (i$1111 >= lim$1110) throw $(Not_found$20g);
           if (oc$$srefu(s$1109, i$1111) === c$1112) return i$1111;
           return __(index_rec$1108, [ s$1109, lim$1110, i$1111 + 1, c$1112 ]);
         });
    var index$1113 = _f(function (s$1114, c$1115) { return __(index_rec$1108, [ s$1114, s$1114.length, 0, c$1115 ]); });
    var index_from$1116 =
      _f(function (s$1117, i$1118, c$1119) {
           var l$1120 = s$1117.length;
           if (i$1118 < 0 || i$1118 > l$1120) return __(oc$Pervasives$[0], [ "String.index_from" ]);
           return __(index_rec$1108, [ s$1117, l$1120, i$1118, c$1119 ]);
         });
    var rindex_rec$1121 =
      _f(function (s$1122, i$1123, c$1124) {
           if (i$1123 < 0) throw $(Not_found$20g);
           if (oc$$srefu(s$1122, i$1123) === c$1124) return i$1123;
           return __(rindex_rec$1121, [ s$1122, i$1123 - 1, c$1124 ]);
         });
    var rindex$1125 = _f(function (s$1126, c$1127) { return __(rindex_rec$1121, [ s$1126, s$1126.length - 1, c$1127 ]); });
    var rindex_from$1128 =
      _f(function (s$1129, i$1130, c$1131) {
           if (i$1130 < -1 || i$1130 >= s$1129.length) return __(oc$Pervasives$[0], [ "String.rindex_from" ]);
           return __(rindex_rec$1121, [ s$1129, i$1130, c$1131 ]);
         });
    var contains_from$1132 =
      _f(function (s$1133, i$1134, c$1135) {
           var l$1136 = s$1133.length;
           if (i$1134 < 0 || i$1134 > l$1136) return __(oc$Pervasives$[0], [ "String.contains_from" ]);
           try {
             _(index_rec$1108, [ s$1133, l$1136, i$1134, c$1135 ]);
             return true;
           }
           catch (exn$1149) {
             if (exn$1149[0] === Not_found$20g) return false;
             throw exn$1149;
           }
         });
    var contains$1137 = _f(function (s$1138, c$1139) { return __(contains_from$1132, [ s$1138, 0, c$1139 ]); });
    var rcontains_from$1140 =
      _f(function (s$1141, i$1142, c$1143) {
           if (i$1142 < 0 || i$1142 >= s$1141.length) return __(oc$Pervasives$[0], [ "String.rcontains_from" ]);
           try {
             _(rindex_rec$1121, [ s$1141, i$1142, c$1143 ]);
             return true;
           }
           catch (exn$1148) {
             if (exn$1148[0] === Not_found$20g) return false;
             throw exn$1148;
           }
         });
    var compare$1145 = _f(function (prim$1147, prim$1146) { return caml_compare(prim$1147, prim$1146); });
    return $(make$1038, copy$1042, sub$1046, fill$1051, blit$1056, concat$1066, iter$1062, escaped$1080, index$1113, rindex$1125,
             index_from$1116, rindex_from$1128, contains$1137, contains_from$1132, rcontains_from$1140, uppercase$1096,
             lowercase$1098, capitalize$1104, uncapitalize$1106, compare$1145);
  }();
var oc$Sys$ =
  function () {
    var match$1090 = caml_sys_get_argv(0);
    var match$1089 = caml_sys_get_config(0);
    var word_size$1035 = match$1089[1];
    var max_array_length$1036 = (1 << word_size$1035 - 10) - 1;
    var max_string_length$1037 = (word_size$1035 / 8 >> 0) * max_array_length$1036 - 1;
    var interactive$1048 = $(false);
    var set_signal$1057 =
      _f(function (sig_num$1058, sig_beh$1059) { caml_install_signal_handler(sig_num$1058, sig_beh$1059); return 0; });
    var sigabrt$1060 = -1;
    var sigalrm$1061 = -2;
    var sigfpe$1062 = -3;
    var sighup$1063 = -4;
    var sigill$1064 = -5;
    var sigint$1065 = -6;
    var sigkill$1066 = -7;
    var sigpipe$1067 = -8;
    var sigquit$1068 = -9;
    var sigsegv$1069 = -10;
    var sigterm$1070 = -11;
    var sigusr1$1071 = -12;
    var sigusr2$1072 = -13;
    var sigchld$1073 = -14;
    var sigcont$1074 = -15;
    var sigstop$1075 = -16;
    var sigtstp$1076 = -17;
    var sigttin$1077 = -18;
    var sigttou$1078 = -19;
    var sigvtalrm$1079 = -20;
    var sigprof$1080 = -21;
    var Break$1081 = $("Sys.Break");
    var catch_break$1082 =
      _f(function (on$1083) {
           if (on$1083) return __(set_signal$1057, [ sigint$1065, $(_f(function (param$1088) { throw $(Break$1081); })) ]);
           return __(set_signal$1057, [ sigint$1065, 0 ]);
         });
    var ocaml_version$1084 = "3.12.0";
    return $(match$1090[1], match$1090[0], interactive$1048, match$1089[0], word_size$1035, max_string_length$1037,
             max_array_length$1036, set_signal$1057, sigabrt$1060, sigalrm$1061, sigfpe$1062, sighup$1063, sigill$1064,
             sigint$1065, sigkill$1066, sigpipe$1067, sigquit$1068, sigsegv$1069, sigterm$1070, sigusr1$1071, sigusr2$1072,
             sigchld$1073, sigcont$1074, sigstop$1075, sigtstp$1076, sigttin$1077, sigttou$1078, sigvtalrm$1079, sigprof$1080,
             Break$1081, catch_break$1082, ocaml_version$1084);
  }();
var oc$Hashtbl$ =
  function () {
    var hash$1031 = _f(function (x$1032) { return caml_hash_univ_param(10, 100, x$1032); });
    var create$1051 =
      _f(function (initial_size$1052) {
           var s$1053 = _(oc$Pervasives$[3], [ _(oc$Pervasives$[4], [ 1, initial_size$1052 ]), oc$Sys$[6] ]);
           return $(0, caml_make_vect(s$1053, 0));
         });
    var clear$1054 =
      _f(function (h$1055) {
           for (var i$1056 = 0; i$1056 <= (h$1055[1]).length - 1; i$1056++) {
             (function (i$1056) { oc$$asets(h$1055[1], i$1056, 0); }(i$1056));
           }
           return h$1055[0] = 0;
         });
    var copy$1057 = _f(function (h$1058) { return $(h$1058[0], _(oc$Array$[6], [ h$1058[1] ])); });
    var length$1059 = _f(function (h$1060) { return h$1060[0]; });
    var resize$1061 =
      _f(function (hashfun$1062, tbl$1063) {
           var odata$1064 = tbl$1063[1];
           var osize$1065 = odata$1064.length;
           var nsize$1066 = _(oc$Pervasives$[3], [ 2 * osize$1065 + 1, oc$Sys$[6] ]);
           if (nsize$1066 !== osize$1065) {
             {
               var ndata$1067 = caml_make_vect(nsize$1066, 0);
               var insert_bucket$1068 =
                 _f(function (param$1302) {
                      if (param$1302) {
                        {
                          var key$1069 = param$1302[0];
                          _(insert_bucket$1068, [ param$1302[2] ]);
                          var nidx$1072 = _(hashfun$1062, [ key$1069 ]) % nsize$1066;
                          return oc$$asets(ndata$1067, nidx$1072, $(key$1069, param$1302[1], oc$$arefs(ndata$1067, nidx$1072)));
                        }
                      }
                      return 0;
                    });
               for (var i$1073 = 0; i$1073 <= osize$1065 - 1; i$1073++) {
                 (function (i$1073) { _(insert_bucket$1068, [ oc$$arefs(odata$1064, i$1073) ]); }(i$1073));
               }
               return tbl$1063[1] = ndata$1067;
             }
           }
           return 0;
         });
    var add$1074 =
      _f(function (h$1075, key$1076, info$1077) {
           var i$1078 = _(hash$1031, [ key$1076 ]) % (h$1075[1]).length;
           var bucket$1079 = $(key$1076, info$1077, oc$$arefs(h$1075[1], i$1078));
           oc$$asets(h$1075[1], i$1078, bucket$1079);
           h$1075[0] = 1 + h$1075[0];
           if (h$1075[0] > (h$1075[1]).length << 1) return __(resize$1061, [ hash$1031, h$1075 ]);
           return 0;
         });
    var remove$1080 =
      _f(function (h$1081, key$1082) {
           var remove_bucket$1083 =
             _f(function (param$1301) {
                  if (param$1301) {
                    {
                      var next$1086 = param$1301[2];
                      var k$1084 = param$1301[0];
                      if (caml_compare(k$1084, key$1082) === 0) { { h$1081[0] = -1 + h$1081[0]; return next$1086; } }
                      return $(k$1084, param$1301[1], _(remove_bucket$1083, [ next$1086 ]));
                    }
                  }
                  return 0;
                });
           var i$1087 = _(hash$1031, [ key$1082 ]) % (h$1081[1]).length;
           return oc$$asets(h$1081[1], i$1087, _(remove_bucket$1083, [ oc$$arefs(h$1081[1], i$1087) ]));
         });
    var find_rec$1088 =
      _f(function (key$1089, param$1300) {
           if (!param$1300) throw $(Not_found$20g);
           if (caml_compare(key$1089, param$1300[0]) === 0) return param$1300[1];
           return __(find_rec$1088, [ key$1089, param$1300[2] ]);
         });
    var find$1093 =
      _f(function (h$1094, key$1095) {
           var match$1299 = oc$$arefs(h$1094[1], _(hash$1031, [ key$1095 ]) % (h$1094[1]).length);
           if (!match$1299) throw $(Not_found$20g);
           if (caml_compare(key$1095, match$1299[0]) === 0) return match$1299[1];
           var rest1$1098 = match$1299[2];
           if (!rest1$1098) throw $(Not_found$20g);
           if (caml_compare(key$1095, rest1$1098[0]) === 0) return rest1$1098[1];
           var rest2$1101 = rest1$1098[2];
           if (!rest2$1101) throw $(Not_found$20g);
           if (caml_compare(key$1095, rest2$1101[0]) === 0) return rest2$1101[1];
           return __(find_rec$1088, [ key$1095, rest2$1101[2] ]);
         });
    var find_all$1105 =
      _f(function (h$1106, key$1107) {
           var find_in_bucket$1108 =
             _f(function (param$1298) {
                  if (param$1298) {
                    {
                      var rest$1111 = param$1298[2];
                      if (caml_compare(param$1298[0], key$1107) === 0)
                        return $(param$1298[1], _(find_in_bucket$1108, [ rest$1111 ]));
                      return __(find_in_bucket$1108, [ rest$1111 ]);
                    }
                  }
                  return 0;
                });
           return __(find_in_bucket$1108, [ oc$$arefs(h$1106[1], _(hash$1031, [ key$1107 ]) % (h$1106[1]).length) ]);
         });
    var replace$1112 =
      _f(function (h$1113, key$1114, info$1115) {
           var replace_bucket$1116 =
             _f(function (param$1297) {
                  if (param$1297) {
                    {
                      var next$1119 = param$1297[2];
                      var k$1117 = param$1297[0];
                      if (caml_compare(k$1117, key$1114) === 0) return $(k$1117, info$1115, next$1119);
                      return $(k$1117, param$1297[1], _(replace_bucket$1116, [ next$1119 ]));
                    }
                  }
                  throw $(Not_found$20g);
                });
           var i$1120 = _(hash$1031, [ key$1114 ]) % (h$1113[1]).length;
           var l$1121 = oc$$arefs(h$1113[1], i$1120);
           try {
             return oc$$asets(h$1113[1], i$1120, _(replace_bucket$1116, [ l$1121 ]));
           }
           catch (exn$1296) {
             if (exn$1296[0] === Not_found$20g) {
               {
                 oc$$asets(h$1113[1], i$1120, $(key$1114, info$1115, l$1121));
                 h$1113[0] = 1 + h$1113[0];
                 if (h$1113[0] > (h$1113[1]).length << 1) return __(resize$1061, [ hash$1031, h$1113 ]);
                 return 0;
               }
             }
             throw exn$1296;
           }
         });
    var mem$1122 =
      _f(function (h$1123, key$1124) {
           var mem_in_bucket$1125 =
             _f(function (param$1295) {
                  if (param$1295) return caml_compare(param$1295[0], key$1124) === 0 || _(mem_in_bucket$1125, [ param$1295[2] ]);
                  return false;
                });
           return __(mem_in_bucket$1125, [ oc$$arefs(h$1123[1], _(hash$1031, [ key$1124 ]) % (h$1123[1]).length) ]);
         });
    var iter$1129 =
      _f(function (f$1130, h$1131) {
           var do_bucket$1132 =
             _f(function (param$1294) {
                  if (param$1294) {
                    { _(f$1130, [ param$1294[0], param$1294[1] ]); return __(do_bucket$1132, [ param$1294[2] ]); }
                  }
                  return 0;
                });
           var d$1136 = h$1131[1];
           for (var i$1137 = 0; i$1137 <= d$1136.length - 1; i$1137++) {
             (function (i$1137) { _(do_bucket$1132, [ oc$$arefs(d$1136, i$1137) ]); }(i$1137));
           }
         });
    var fold$1138 =
      _f(function (f$1139, h$1140, init$1141) {
           var do_bucket$1142 =
             _f(function (b$1143, accu$1144) {
                  if (b$1143) return __(do_bucket$1142, [ b$1143[2], _(f$1139, [ b$1143[0], b$1143[1], accu$1144 ]) ]);
                  return accu$1144;
                });
           var d$1148 = h$1140[1];
           var accu$1149 = init$1141;
           for (var i$1150 = 0; i$1150 <= d$1148.length - 1; i$1150++) {
             (function (i$1150) { accu$1149 = _(do_bucket$1142, [ oc$$arefs(d$1148, i$1150), accu$1149 ]); }(i$1150));
           }
           return accu$1149;
         });
    var Make$1251 =
      _f(function (H$1170) {
           var safehash$1177 = _f(function (key$1178) { return _(H$1170[1], [ key$1178 ]) & oc$Pervasives$[6]; });
           var add$1179 =
             _f(function (h$1180, key$1181, info$1182) {
                  var i$1183 = _(safehash$1177, [ key$1181 ]) % (h$1180[1]).length;
                  var bucket$1184 = $(key$1181, info$1182, oc$$arefs(h$1180[1], i$1183));
                  oc$$asets(h$1180[1], i$1183, bucket$1184);
                  h$1180[0] = 1 + h$1180[0];
                  if (h$1180[0] > (h$1180[1]).length << 1) return __(resize$1061, [ safehash$1177, h$1180 ]);
                  return 0;
                });
           var remove$1185 =
             _f(function (h$1186, key$1187) {
                  var remove_bucket$1188 =
                    _f(function (param$1293) {
                         if (param$1293) {
                           {
                             var next$1191 = param$1293[2];
                             var k$1189 = param$1293[0];
                             if (_(H$1170[0], [ k$1189, key$1187 ])) { { h$1186[0] = -1 + h$1186[0]; return next$1191; } }
                             return $(k$1189, param$1293[1], _(remove_bucket$1188, [ next$1191 ]));
                           }
                         }
                         return 0;
                       });
                  var i$1192 = _(safehash$1177, [ key$1187 ]) % (h$1186[1]).length;
                  return oc$$asets(h$1186[1], i$1192, _(remove_bucket$1188, [ oc$$arefs(h$1186[1], i$1192) ]));
                });
           var find_rec$1193 =
             _f(function (key$1194, param$1292) {
                  if (!param$1292) throw $(Not_found$20g);
                  if (_(H$1170[0], [ key$1194, param$1292[0] ])) return param$1292[1];
                  return __(find_rec$1193, [ key$1194, param$1292[2] ]);
                });
           var find$1198 =
             _f(function (h$1199, key$1200) {
                  var match$1291 = oc$$arefs(h$1199[1], _(safehash$1177, [ key$1200 ]) % (h$1199[1]).length);
                  if (match$1291) {
                    {
                      var rest1$1203 = match$1291[2];
                      if (_(H$1170[0], [ key$1200, match$1291[0] ])) return match$1291[1];
                      if (rest1$1203) {
                        {
                          var rest2$1206 = rest1$1203[2];
                          if (_(H$1170[0], [ key$1200, rest1$1203[0] ])) return rest1$1203[1];
                          if (!rest2$1206) throw $(Not_found$20g);
                          if (_(H$1170[0], [ key$1200, rest2$1206[0] ])) return rest2$1206[1];
                          return __(find_rec$1193, [ key$1200, rest2$1206[2] ]);
                        }
                      }
                      throw $(Not_found$20g);
                    }
                  }
                  throw $(Not_found$20g);
                });
           var find_all$1210 =
             _f(function (h$1211, key$1212) {
                  var find_in_bucket$1213 =
                    _f(function (param$1290) {
                         if (param$1290) {
                           {
                             var rest$1216 = param$1290[2];
                             if (_(H$1170[0], [ param$1290[0], key$1212 ]))
                               return $(param$1290[1], _(find_in_bucket$1213, [ rest$1216 ]));
                             return __(find_in_bucket$1213, [ rest$1216 ]);
                           }
                         }
                         return 0;
                       });
                  return __(find_in_bucket$1213, [ oc$$arefs(h$1211[1], _(safehash$1177, [ key$1212 ]) % (h$1211[1]).length) ]);
                });
           var replace$1217 =
             _f(function (h$1218, key$1219, info$1220) {
                  var replace_bucket$1221 =
                    _f(function (param$1289) {
                         if (param$1289) {
                           {
                             var next$1224 = param$1289[2];
                             var k$1222 = param$1289[0];
                             if (_(H$1170[0], [ k$1222, key$1219 ])) return $(k$1222, info$1220, next$1224);
                             return $(k$1222, param$1289[1], _(replace_bucket$1221, [ next$1224 ]));
                           }
                         }
                         throw $(Not_found$20g);
                       });
                  var i$1225 = _(safehash$1177, [ key$1219 ]) % (h$1218[1]).length;
                  var l$1226 = oc$$arefs(h$1218[1], i$1225);
                  try {
                    return oc$$asets(h$1218[1], i$1225, _(replace_bucket$1221, [ l$1226 ]));
                  }
                  catch (exn$1288) {
                    if (exn$1288[0] === Not_found$20g) {
                      {
                        oc$$asets(h$1218[1], i$1225, $(key$1219, info$1220, l$1226));
                        h$1218[0] = 1 + h$1218[0];
                        if (h$1218[0] > (h$1218[1]).length << 1) return __(resize$1061, [ safehash$1177, h$1218 ]);
                        return 0;
                      }
                    }
                    throw exn$1288;
                  }
                });
           var mem$1227 =
             _f(function (h$1228, key$1229) {
                  var mem_in_bucket$1230 =
                    _f(function (param$1287) {
                         if (param$1287)
                           return _(H$1170[0], [ param$1287[0], key$1229 ]) || _(mem_in_bucket$1230, [ param$1287[2] ]);
                         return false;
                       });
                  return __(mem_in_bucket$1230, [ oc$$arefs(h$1228[1], _(safehash$1177, [ key$1229 ]) % (h$1228[1]).length) ]);
                });
           return $(create$1051, clear$1054, copy$1057, add$1179, remove$1185, find$1198, find_all$1210, replace$1217, mem$1227,
                    iter$1129, fold$1138, length$1059);
         });
    return $(create$1051, clear$1054, add$1074, copy$1057, find$1093, find_all$1105, mem$1122, remove$1080, replace$1112,
             iter$1129, fold$1138, length$1059, Make$1251, hash$1031);
  }();
var oc$Int64$ =
  function () {
    var zero$1050 = "0";
    var one$1051 = "1";
    var minus_one$1052 = "-1";
    var succ$1053 = _f(function (n$1054) { return n$1054 + "1"; });
    var pred$1055 = _f(function (n$1056) { return n$1056 - "1"; });
    var abs$1057 = _f(function (n$1058) { if (n$1058 >= "0") return n$1058; return -n$1058; });
    var min_int$1059 = "-9223372036854775808";
    var max_int$1060 = "9223372036854775807";
    var lognot$1061 = _f(function (n$1062) { return n$1062 ^ "-1"; });
    var to_string$1064 = _f(function (n$1065) { return caml_format_int("%d", n$1065); });
    var compare$1070 = _f(function (x$1071, y$1072) { return caml_int64_compare(x$1071, y$1072); });
    return $(zero$1050, one$1051, minus_one$1052, succ$1053, pred$1055, abs$1057, max_int$1060, min_int$1059, lognot$1061,
             to_string$1064, compare$1070);
  }();
var oc$Stack$ =
  function () {
    var Empty$1034 = $("Stack.Empty");
    var create$1035 = _f(function (param$1062) { return $(0); });
    var clear$1036 = _f(function (s$1037) { return s$1037[0] = 0; });
    var copy$1038 = _f(function (s$1039) { return $(s$1039[0]); });
    var push$1040 = _f(function (x$1041, s$1042) { return s$1042[0] = $(x$1041, s$1042[0]); });
    var pop$1043 =
      _f(function (s$1044) {
           var match$1060 = s$1044[0];
           if (match$1060) { { s$1044[0] = match$1060[1]; return match$1060[0]; } }
           throw $(Empty$1034);
         });
    var top$1047 = _f(function (s$1048) { var match$1058 = s$1048[0]; if (match$1058) return match$1058[0]; throw $(Empty$1034); });
    var is_empty$1050 = _f(function (s$1051) { return s$1051[0] === 0; });
    var length$1052 = _f(function (s$1053) { return __(oc$List$[0], [ s$1053[0] ]); });
    var iter$1054 = _f(function (f$1055, s$1056) { return __(oc$List$[9], [ f$1055, s$1056[0] ]); });
    return $(Empty$1034, create$1035, push$1040, pop$1043, top$1047, clear$1036, copy$1038, is_empty$1050, length$1052, iter$1054);
  }();
var oc$Queue$ =
  function () {
    var Empty$1030 = $("Queue.Empty");
    var create$1047 = _f(function (param$1108) { return $(0, 0); });
    var clear$1048 = _f(function (q$1049) { q$1049[0] = 0; return q$1049[1] = 0; });
    var add$1050 =
      _f(function (x$1051, q$1052) {
           q$1052[0] = q$1052[0] + 1;
           if (q$1052[0] === 1) {
             { var cell$1053 = $(x$1051, cell$1053); cell$1053[1] = cell$1053; return q$1052[1] = cell$1053; }
           }
           var tail$1054 = q$1052[1];
           var head$1055 = tail$1054[1];
           var cell$1056 = $(x$1051, head$1055);
           tail$1054[1] = cell$1056;
           return q$1052[1] = cell$1056;
         });
    var peek$1058 = _f(function (q$1059) { if (q$1059[0] === 0) throw $(Empty$1030); return q$1059[1][1][0]; });
    var take$1061 =
      _f(function (q$1062) {
           if (q$1062[0] === 0) throw $(Empty$1030); else;
           q$1062[0] = q$1062[0] - 1;
           var tail$1063 = q$1062[1];
           var head$1064 = tail$1063[1];
           if (head$1064 === tail$1063) q$1062[1] = 0; else tail$1063[1] = head$1064[1];
           return head$1064[0];
         });
    var copy$1066 =
      _f(function (q$1067) {
           if (q$1067[0] === 0) return __(create$1047, [ 0 ]);
           var tail$1068 = q$1067[1];
           var tail$27$1069 = $(tail$1068[0], tail$27$1069);
           tail$27$1069[1] = tail$27$1069;
           var copy$1070 =
             _f(function (cell$1071) {
                  if (cell$1071 === tail$1068) return tail$27$1069;
                  return $(cell$1071[0], _(copy$1070, [ cell$1071[1] ]));
                });
           tail$27$1069[1] = _(copy$1070, [ tail$1068[1] ]);
           return $(q$1067[0], tail$27$1069);
         });
    var is_empty$1072 = _f(function (q$1073) { return q$1073[0] === 0; });
    var length$1074 = _f(function (q$1075) { return q$1075[0]; });
    var iter$1076 =
      _f(function (f$1077, q$1078) {
           if (q$1078[0] > 0) {
             {
               var tail$1079 = q$1078[1];
               var iter$1080 =
                 _f(function (cell$1081) {
                      _(f$1077, [ cell$1081[0] ]);
                      if (cell$1081 !== tail$1079) return __(iter$1080, [ cell$1081[1] ]);
                      return 0;
                    });
               return __(iter$1080, [ tail$1079[1] ]);
             }
           }
           return 0;
         });
    var fold$1082 =
      _f(function (f$1083, accu$1084, q$1085) {
           if (q$1085[0] === 0) return accu$1084;
           var tail$1086 = q$1085[1];
           var fold$1087 =
             _f(function (accu$1088, cell$1089) {
                  var accu$1090 = _(f$1083, [ accu$1088, cell$1089[0] ]);
                  if (cell$1089 === tail$1086) return accu$1090;
                  return __(fold$1087, [ accu$1090, cell$1089[1] ]);
                });
           return __(fold$1087, [ accu$1084, tail$1086[1] ]);
         });
    var transfer$1091 =
      _f(function (q1$1092, q2$1093) {
           var length1$1094 = q1$1092[0];
           if (length1$1094 > 0) {
             {
               var tail1$1095 = q1$1092[1];
               _(clear$1048, [ q1$1092 ]);
               if (q2$1093[0] > 0) {
                 {
                   var tail2$1096 = q2$1093[1];
                   var head1$1097 = tail1$1095[1];
                   var head2$1098 = tail2$1096[1];
                   tail1$1095[1] = head2$1098;
                   tail2$1096[1] = head1$1097;
                 }
               }
               else;
               q2$1093[0] = q2$1093[0] + length1$1094;
               return q2$1093[1] = tail1$1095;
             }
           }
           return 0;
         });
    return $(Empty$1030, create$1047, add$1050, add$1050, take$1061, take$1061, peek$1058, peek$1058, clear$1048, copy$1066,
             is_empty$1072, length$1074, iter$1076, fold$1082, transfer$1091);
  }();
var oc$Buffer$ =
  function () {
    var create$1054 = _f(function (n$1055) { return $(new Array(), 0, 0); });
    var contents$1056 =
      _f(function (b$1057) {
           var match$1149 = b$1057[1];
           if (match$1149) return match$1149[0];
           var s$1059 = (b$1057[0]).join('');
           b$1057[1] = $(s$1059);
           return s$1059;
         });
    var sub$1060 =
      _f(function (b$1061, ofs$1062, len$1063) {
           if (ofs$1062 < 0 || (len$1063 < 0 || ofs$1062 > b$1061[2] - len$1063)) _(oc$Pervasives$[0], [ "Buffer.sub" ]); else;
           var s$1064 = _(contents$1056, [ b$1061 ]);
           return s$1064.substring(ofs$1062, ofs$1062 + len$1063);
         });
    var blit$1065 =
      _f(function (src$1066, srcoff$1067, dst$1068, dstoff$1069, len$1070) { return __(oc$Pervasives$[1], [ "unimplemented" ]); });
    var nth$1071 =
      _f(function (b$1072, ofs$1073) {
           if (ofs$1073 < 0 || ofs$1073 >= b$1072[2]) _(oc$Pervasives$[0], [ "Buffer.nth" ]); else;
           var s$1074 = _(contents$1056, [ b$1072 ]);
           return s$1074.charCodeAt(ofs$1073);
         });
    var length$1075 = _f(function (b$1076) { return b$1076[2]; });
    var clear$1077 = _f(function (b$1078) { b$1078[2] = 0; (b$1078[0]).length = 0; });
    var add_char$1080 =
      _f(function (b$1081, c$1082) { b$1081[1] = 0; b$1081[2] = b$1081[2] + 1; (b$1081[0]).push(String.fromCharCode(c$1082)); });
    var add_substring$1083 =
      _f(function (b$1084, s$1085, offset$1086, len$1087) {
           if (offset$1086 < 0 || (len$1087 < 0 || offset$1086 > s$1085.length - len$1087))
             _(oc$Pervasives$[0], [ "Buffer.add_substring" ]);
           else;
           b$1084[1] = 0;
           b$1084[2] = b$1084[2] + len$1087;
           (b$1084[0]).push(s$1085.substring(offset$1086, offset$1086 + len$1087));
         });
    var add_string$1088 =
      _f(function (b$1089, s$1090) { b$1089[1] = 0; b$1089[2] = b$1089[2] + s$1090.length; (b$1089[0]).push(s$1090); });
    var add_buffer$1091 = _f(function (b$1092, bs$1093) { return __(add_string$1088, [ b$1092, _(contents$1056, [ bs$1093 ]) ]); });
    var add_channel$1094 = _f(function (b$1095, ic$1096, len$1097) { return __(oc$Pervasives$[1], [ "unsupported" ]); });
    var output_buffer$1098 = _f(function (oc$1099, b$1100) { return __(oc$Pervasives$[1], [ "unsupported" ]); });
    var closing$1101 =
      _f(function (param$1148) {
           if (!(param$1148 !== 40)) return 41;
           if (param$1148 !== 123) throw $(Assert_failure$26g, $("buffer.ml", 120, 9));
           return 125;
         });
    var advance_to_closing$1102 =
      _f(function (opening$1103, closing$1104, k$1105, s$1106, start$1107) {
           var advance$1108 =
             _f(function (k$1109, i$1110, lim$1111) {
                  if (i$1110 >= lim$1111) throw $(Not_found$20g);
                  if (oc$$srefs(s$1106, i$1110) === opening$1103) return __(advance$1108, [ k$1109 + 1, i$1110 + 1, lim$1111 ]);
                  if (!(oc$$srefs(s$1106, i$1110) === closing$1104)) return __(advance$1108, [ k$1109, i$1110 + 1, lim$1111 ]);
                  if (k$1109 === 0) return i$1110;
                  return __(advance$1108, [ k$1109 - 1, i$1110 + 1, lim$1111 ]);
                });
           return __(advance$1108, [ k$1105, start$1107, s$1106.length ]);
         });
    var advance_to_non_alpha$1112 =
      _f(function (s$1113, start$1114) {
           var advance$1115 =
             _f(function (i$1116, lim$1117) {
                  if (i$1116 >= lim$1117) return lim$1117;
                  var match$1145 = oc$$srefs(s$1113, i$1116);
                  var $r15 = false;
                  r$15: {
                    {
                      if (!(match$1145 < 95)) {
                        {
                          if (!(match$1145 >= 123)) { { if (match$1145 !== 96) { { $r15 = true; break r$15; } } return i$1116; } }
                          if (match$1145 >= 192) {
                            {
                              var s$1151 = -192 + match$1145;
                              switch (s$1151)
                              {
                              case 0: $r15 = true; break r$15;
                              case 1: $r15 = true; break r$15;
                              case 2: $r15 = true; break r$15;
                              case 3: return i$1116;
                              case 4: return i$1116;
                              case 5: return i$1116;
                              case 6: return i$1116;
                              case 7: $r15 = true; break r$15;
                              case 8: $r15 = true; break r$15;
                              case 9: $r15 = true; break r$15;
                              case 10: $r15 = true; break r$15;
                              case 11: $r15 = true; break r$15;
                              case 12: return i$1116;
                              case 13: return i$1116;
                              case 14: $r15 = true; break r$15;
                              case 15: $r15 = true; break r$15;
                              case 16: return i$1116;
                              case 17: return i$1116;
                              case 18: return i$1116;
                              case 19: return i$1116;
                              case 20: $r15 = true; break r$15;
                              case 21: return i$1116;
                              case 22: return i$1116;
                              case 23: return i$1116;
                              case 24: return i$1116;
                              case 25: $r15 = true; break r$15;
                              case 26: return i$1116;
                              case 27: $r15 = true; break r$15;
                              case 28: $r15 = true; break r$15;
                              case 29: return i$1116;
                              case 30: return i$1116;
                              case 31: return i$1116;
                              case 32: $r15 = true; break r$15;
                              case 33: $r15 = true; break r$15;
                              case 34: $r15 = true; break r$15;
                              case 35: return i$1116;
                              case 36: return i$1116;
                              case 37: return i$1116;
                              case 38: return i$1116;
                              case 39: $r15 = true; break r$15;
                              case 40: $r15 = true; break r$15;
                              case 41: $r15 = true; break r$15;
                              case 42: $r15 = true; break r$15;
                              case 43: $r15 = true; break r$15;
                              case 44: return i$1116;
                              case 45: return i$1116;
                              case 46: $r15 = true; break r$15;
                              case 47: $r15 = true; break r$15;
                              case 48: return i$1116;
                              case 49: return i$1116;
                              case 50: return i$1116;
                              case 51: return i$1116;
                              case 52: $r15 = true; break r$15;
                              case 53: return i$1116;
                              case 54: return i$1116;
                              case 55: return i$1116;
                              case 56: return i$1116;
                              case 57: $r15 = true; break r$15;
                              case 58: return i$1116;
                              case 59: $r15 = true; break r$15;
                              case 60: $r15 = true; break r$15;
                              case 61: return i$1116;
                              case 62: return i$1116;
                              case 63: return i$1116;
                              default: return null;
                              }
                            }
                          }
                          return i$1116;
                        }
                      }
                      if (!(match$1145 >= 58)) { { if (match$1145 >= 48) { { $r15 = true; break r$15; } } return i$1116; } }
                      if (!(-65 + match$1145 < 0 || -65 + match$1145 > 25)) { { $r15 = true; break r$15; } }
                      return i$1116;
                    }
                  }
                  if ($r15) return __(advance$1115, [ i$1116 + 1, lim$1117 ]);
                });
           return __(advance$1115, [ start$1114, s$1113.length ]);
         });
    var find_ident$1118 =
      _f(function (s$1119, start$1120, lim$1121) {
           if (start$1120 >= lim$1121) throw $(Not_found$20g);
           var c$1122 = oc$$srefs(s$1119, start$1120);
           var $r12 = false;
           r$12: {
             {
               if (!(c$1122 !== 40)) { { $r12 = true; break r$12; } }
               if (!(c$1122 !== 123)) { { $r12 = true; break r$12; } }
               var stop$1125 = _(advance_to_non_alpha$1112, [ s$1119, start$1120 + 1 ]);
               return $(_(oc$String$[2], [ s$1119, start$1120, stop$1125 - start$1120 ]), stop$1125);
             }
           }
           if ($r12) {
             {
               var new_start$1123 = start$1120 + 1;
               var stop$1124 = _(advance_to_closing$1102, [ c$1122, _(closing$1101, [ c$1122 ]), 0, s$1119, new_start$1123 ]);
               return $(_(oc$String$[2], [ s$1119, new_start$1123, stop$1124 - start$1120 - 1 ]), stop$1124 + 1);
             }
           }
         });
    var add_substitute$1126 =
      _f(function (b$1127, f$1128, s$1129) {
           var lim$1130 = s$1129.length;
           var subst$1131 =
             _f(function (previous$1132, i$1133) {
                  if (i$1133 < lim$1130) {
                    {
                      var current$1134 = oc$$srefs(s$1129, i$1133);
                      if (!(current$1134 !== 36)) {
                        {
                          if (previous$1132 === 92) {
                            { _(add_char$1080, [ b$1127, current$1134 ]); return __(subst$1131, [ 32, i$1133 + 1 ]); }
                          }
                          var j$1138 = i$1133 + 1;
                          var match$1144 = _(find_ident$1118, [ s$1129, j$1138, lim$1130 ]);
                          _(add_string$1088, [ b$1127, _(f$1128, [ match$1144[0] ]) ]);
                          return __(subst$1131, [ 32, match$1144[1] ]);
                        }
                      }
                      if (previous$1132 === 92) {
                        {
                          _(add_char$1080, [ b$1127, 92 ]);
                          _(add_char$1080, [ b$1127, current$1134 ]);
                          return __(subst$1131, [ 32, i$1133 + 1 ]);
                        }
                      }
                      if (current$1134 !== 92) {
                        { _(add_char$1080, [ b$1127, current$1134 ]); return __(subst$1131, [ current$1134, i$1133 + 1 ]); }
                      }
                      return __(subst$1131, [ current$1134, i$1133 + 1 ]);
                    }
                  }
                  if (previous$1132 === 92) return __(add_char$1080, [ b$1127, previous$1132 ]);
                  return 0;
                });
           return __(subst$1131, [ 32, 0 ]);
         });
    return $(create$1054, contents$1056, sub$1060, blit$1065, nth$1071, length$1075, clear$1077, clear$1077, add_char$1080,
             add_string$1088, add_substring$1083, add_substitute$1126, add_buffer$1091, add_channel$1094, output_buffer$1098);
  }();
var oc$Printf$ =
  function () {
    var Sformat$1056 =
      function () {
        var index_of_int$1037 =
          _f(function (i$1038) {
               if (i$1038 >= 0) return i$1038;
               return __(oc$Pervasives$[1],
                         [
                           _(oc$Pervasives$[15], [ "Sformat.index_of_int: negative argument ", _(oc$Pervasives$[19], [ i$1038 ]) ])
                         ]);
             });
        var add_int_index$1040 = _f(function (i$1041, idx$1042) { return __(index_of_int$1037, [ i$1041 + idx$1042 ]); });
        var succ_index$1043 = _(add_int_index$1040, [ 1 ]);
        var index_of_literal_position$1044 = _f(function (p$1045) { return __(index_of_int$1037, [ -1 + p$1045 ]); });
        var sub$1050 = _f(function (fmt$1051, idx$1052, len$1053) { return __(oc$String$[2], [ fmt$1051, idx$1052, len$1053 ]); });
        var to_string$1054 = _f(function (fmt$1055) { return __(sub$1050, [ fmt$1055, 0, fmt$1055.length ]); });
        return $(index_of_int$1037, add_int_index$1040, succ_index$1043, index_of_literal_position$1044, sub$1050, to_string$1054);
      }();
    var bad_conversion$1057 =
      _f(function (sfmt$1058, i$1059, c$1060) {
           return __(oc$Pervasives$[0],
                     [
                       _(oc$Pervasives$[15],
                         [
                           "Printf: bad conversion %",
                           _(oc$Pervasives$[15],
                             [
                               _(oc$String$[0], [ 1, c$1060 ]),
                               _(oc$Pervasives$[15],
                                 [
                                   ", at char number ",
                                   _(oc$Pervasives$[15],
                                     [
                                       _(oc$Pervasives$[19], [ i$1059 ]),
                                       _(oc$Pervasives$[15],
                                         [ " in format string ``", _(oc$Pervasives$[15], [ sfmt$1058, "\'\'" ]) ])
                                     ])
                                 ])
                             ])
                         ])
                     ]);
         });
    var bad_conversion_format$1061 =
      _f(function (fmt$1062, i$1063, c$1064) {
           return __(bad_conversion$1057, [ _(Sformat$1056[5], [ fmt$1062 ]), i$1063, c$1064 ]);
         });
    var incomplete_format$1065 =
      _f(function (fmt$1066) {
           return __(oc$Pervasives$[0],
                     [
                       _(oc$Pervasives$[15],
                         [
                           "Printf: premature end of format string ``",
                           _(oc$Pervasives$[15], [ _(Sformat$1056[5], [ fmt$1066 ]), "\'\'" ])
                         ])
                     ]);
         });
    var parse_string_conversion$1067 =
      _f(function (sfmt$1068) {
           var parse$1069 =
             _f(function (neg$1070, i$1071) {
                  if (i$1071 >= sfmt$1068.length) return $(0, neg$1070);
                  var match$1459 = oc$$srefu(sfmt$1068, i$1071);
                  var $r155 = false;
                  r$155: {
                    {
                      if (!(match$1459 >= 49)) {
                        { if (match$1459 !== 45) { { $r155 = true; break r$155; } } return __(parse$1069, [ true, 1 + i$1071 ]); }
                      }
                      if (match$1459 >= 58) { { $r155 = true; break r$155; } }
                      return $(caml_int_of_string(_(oc$String$[2], [ sfmt$1068, i$1071, sfmt$1068.length - i$1071 - 1 ])),
                               neg$1070);
                    }
                  }
                  if ($r155) return __(parse$1069, [ neg$1070, 1 + i$1071 ]);
                });
           try {
             return _(parse$1069, [ false, 1 ]);
           }
           catch (exn$1457) {
             if (exn$1457[0] === Failure$19g) return __(bad_conversion$1057, [ sfmt$1068, 0, 115 ]);
             throw exn$1457;
           }
         });
    var pad_string$1072 =
      _f(function (pad_char$1073, p$1074, neg$1075, s$1076, i$1077, len$1078) {
           if (p$1074 === len$1078 && i$1077 === 0) return s$1076;
           if (p$1074 <= len$1078) return __(oc$String$[2], [ s$1076, i$1077, len$1078 ]);
           var res$1079 = _(oc$String$[0], [ p$1074, pad_char$1073 ]);
           if (neg$1075)
             _(oc$String$[4], [ s$1076, i$1077, res$1079, 0, len$1078 ]);
           else
             _(oc$String$[4], [ s$1076, i$1077, res$1079, p$1074 - len$1078, len$1078 ]);
           return res$1079;
         });
    var format_string$1080 =
      _f(function (sfmt$1081, s$1082) {
           var match$1456 = _(parse_string_conversion$1067, [ sfmt$1081 ]);
           return __(pad_string$1072, [ 32, match$1456[0], match$1456[1], s$1082, 0, s$1082.length ]);
         });
    var extract_format$1085 =
      _f(function (fmt$1086, start$1087, stop$1088, widths$1089) {
           var skip_positional_spec$1090 =
             _f(function (start$1091) {
                  var match$1454 = oc$$srefu(fmt$1086, start$1091);
                  if (-48 + match$1454 < 0 || -48 + match$1454 > 9) return start$1091;
                  var skip_int_literal$1092 =
                    _f(function (i$1093) {
                         var match$1453 = oc$$srefu(fmt$1086, i$1093);
                         if (!(match$1453 >= 48)) { { if (match$1453 !== 36) return start$1091; return 1 + i$1093; } }
                         if (match$1453 >= 58) return start$1091;
                         return __(skip_int_literal$1092, [ 1 + i$1093 ]);
                       });
                  return __(skip_int_literal$1092, [ 1 + start$1091 ]);
                });
           var start$1094 = _(skip_positional_spec$1090, [ 1 + start$1087 ]);
           var b$1095 = _(oc$Buffer$[0], [ stop$1088 - start$1094 + 10 ]);
           _(oc$Buffer$[8], [ b$1095, 37 ]);
           var fill_format$1096 =
             _f(function (i$1097, widths$1098) {
                  if (i$1097 <= stop$1088) {
                    {
                      var match$1450 = oc$$srefu(fmt$1086, i$1097);
                      if (match$1450 !== 42) {
                        { _(oc$Buffer$[8], [ b$1095, match$1450 ]); return __(fill_format$1096, [ 1 + i$1097, widths$1098 ]); }
                      }
                      if (widths$1098) {
                        {
                          _(oc$Buffer$[9], [ b$1095, _(oc$Pervasives$[19], [ widths$1098[0] ]) ]);
                          var i$1102 = _(skip_positional_spec$1090, [ 1 + i$1097 ]);
                          return __(fill_format$1096, [ i$1102, widths$1098[1] ]);
                        }
                      }
                      throw $(Assert_failure$26g, $("printf.ml", 164, 8));
                    }
                  }
                  return 0;
                });
           _(fill_format$1096, [ start$1094, _(oc$List$[4], [ widths$1089 ]) ]);
           return __(oc$Buffer$[1], [ b$1095 ]);
         });
    var sub_format$1103 =
      _f(function (incomplete_format$1104, bad_conversion_format$1105, conv$1106, fmt$1107, i$1108) {
           var len$1109 = fmt$1107.length;
           var sub_fmt$1110 =
             _f(function (c$1111, i$1112) {
                  var close$1113 = c$1111 === 40 ? 41 : 125;
                  var sub$1114 =
                    _f(function (j$1116) {
                         if (j$1116 >= len$1109) return __(incomplete_format$1104, [ fmt$1107 ]);
                         var match$1447 = oc$$srefs(fmt$1107, j$1116);
                         if (match$1447 !== 37) return __(sub$1114, [ 1 + j$1116 ]);
                         return __(sub_sub$1115, [ 1 + j$1116 ]);
                       });
                  var sub_sub$1115 =
                    _f(function (j$1117) {
                         if (j$1117 >= len$1109) return __(incomplete_format$1104, [ fmt$1107 ]);
                         var c$1118 = oc$$srefs(fmt$1107, j$1117);
                         var $r134 = false;
                         r$134: {
                           {
                             var $r133 = false;
                             r$133: {
                               {
                                 var $r135 = false;
                                 r$135: {
                                   {
                                     var switcher$1448 = -40 + c$1118;
                                     if (switcher$1448 < 0 || switcher$1448 > 1) {
                                       {
                                         var switcher$1449 = -83 + switcher$1448;
                                         if (switcher$1449 < 0 || switcher$1449 > 2) { { $r135 = true; break r$135; } }
                                         switch (switcher$1449)
                                         {
                                         case 0: $r133 = true; break r$133;
                                         case 1: $r135 = true; break r$135;
                                         case 2: $r134 = true; break r$134;
                                         default: return null;
                                         }
                                       }
                                     }
                                     if (!!!switcher$1448) { { $r133 = true; break r$133; } }
                                     $r134 = true;
                                     break r$134;
                                   }
                                 }
                                 if ($r135) return __(sub$1114, [ 1 + j$1117 ]);
                               }
                             }
                             if ($r133) {
                               { var j$1120 = _(sub_fmt$1110, [ c$1118, 1 + j$1117 ]); return __(sub$1114, [ 1 + j$1120 ]); }
                             }
                           }
                         }
                         if ($r134) {
                           {
                             if (c$1118 === close$1113) return 1 + j$1117;
                             return __(bad_conversion_format$1105, [ fmt$1107, i$1112, c$1118 ]);
                           }
                         }
                       });
                  return __(sub$1114, [ i$1112 ]);
                });
           return __(sub_fmt$1110, [ conv$1106, i$1108 ]);
         });
    var sub_format_for_printf$1121 =
      _f(function (conv$1122) { return __(sub_format$1103, [ incomplete_format$1065, bad_conversion_format$1061, conv$1122 ]); });
    var iter_on_format_args$1123 =
      _f(function (fmt$1124, add_conv$1125, add_char$1126) {
           var lim$1127 = fmt$1124.length - 1;
           var scan_flags$1128 =
             _f(function (skip$1131, i$1132) {
                  if (i$1132 > lim$1127) return __(incomplete_format$1065, [ fmt$1124 ]);
                  var match$1444 = oc$$srefu(fmt$1124, i$1132);
                  var $r111 = false;
                  r$111: {
                    {
                      var $r110 = false;
                      r$110: {
                        {
                          var $r112 = false;
                          r$112: {
                            {
                              if (!(match$1444 >= 58)) {
                                {
                                  if (!(match$1444 >= 32)) { { $r112 = true; break r$112; } }
                                  var s$1460 = -32 + match$1444;
                                  switch (s$1460)
                                  {
                                  case 0: $r110 = true; break r$110;
                                  case 1: $r112 = true; break r$112;
                                  case 2: $r112 = true; break r$112;
                                  case 3: $r110 = true; break r$110;
                                  case 4: $r112 = true; break r$112;
                                  case 5: $r112 = true; break r$112;
                                  case 6: $r112 = true; break r$112;
                                  case 7: $r112 = true; break r$112;
                                  case 8: $r112 = true; break r$112;
                                  case 9: $r112 = true; break r$112;
                                  case 10: return __(scan_flags$1128, [ skip$1131, _(add_conv$1125, [ skip$1131, i$1132, 105 ]) ]);
                                  case 11: $r110 = true; break r$110;
                                  case 12: $r112 = true; break r$112;
                                  case 13: $r110 = true; break r$110;
                                  case 14: $r111 = true; break r$111;
                                  case 15: $r112 = true; break r$112;
                                  case 16: $r111 = true; break r$111;
                                  case 17: $r111 = true; break r$111;
                                  case 18: $r111 = true; break r$111;
                                  case 19: $r111 = true; break r$111;
                                  case 20: $r111 = true; break r$111;
                                  case 21: $r111 = true; break r$111;
                                  case 22: $r111 = true; break r$111;
                                  case 23: $r111 = true; break r$111;
                                  case 24: $r111 = true; break r$111;
                                  case 25: $r111 = true; break r$111;
                                  default: return null;
                                  }
                                }
                              }
                              if (match$1444 !== 95) { { $r112 = true; break r$112; } }
                              return __(scan_flags$1128, [ true, 1 + i$1132 ]);
                            }
                          }
                          if ($r112) return __(scan_conv$1129, [ skip$1131, i$1132 ]);
                        }
                      }
                      if ($r110) return __(scan_flags$1128, [ skip$1131, 1 + i$1132 ]);
                    }
                  }
                  if ($r111) return __(scan_flags$1128, [ skip$1131, 1 + i$1132 ]);
                });
           var scan_conv$1129 =
             _f(function (skip$1133, i$1134) {
                  if (i$1134 > lim$1127) return __(incomplete_format$1065, [ fmt$1124 ]);
                  var conv$1135 = oc$$srefu(fmt$1124, i$1134);
                  var $r126 = false;
                  r$126: {
                    {
                      var $r125 = false;
                      r$125: {
                        {
                          var $r124 = false;
                          r$124: {
                            {
                              var $r123 = false;
                              r$123: {
                                {
                                  var $r122 = false;
                                  r$122: {
                                    {
                                      var $r121 = false;
                                      r$121: {
                                        {
                                          var $r120 = false;
                                          r$120: {
                                            {
                                              var $r119 = false;
                                              r$119: {
                                                {
                                                  var $r118 = false;
                                                  r$118: {
                                                    {
                                                      var $r127 = false;
                                                      r$127: {
                                                        {
                                                          if (conv$1135 >= 126) { { $r127 = true; break r$127; } }
                                                          switch (conv$1135)
                                                          {
                                                          case 0: $r127 = true; break r$127;
                                                          case 1: $r127 = true; break r$127;
                                                          case 2: $r127 = true; break r$127;
                                                          case 3: $r127 = true; break r$127;
                                                          case 4: $r127 = true; break r$127;
                                                          case 5: $r127 = true; break r$127;
                                                          case 6: $r127 = true; break r$127;
                                                          case 7: $r127 = true; break r$127;
                                                          case 8: $r127 = true; break r$127;
                                                          case 9: $r127 = true; break r$127;
                                                          case 10: $r127 = true; break r$127;
                                                          case 11: $r127 = true; break r$127;
                                                          case 12: $r127 = true; break r$127;
                                                          case 13: $r127 = true; break r$127;
                                                          case 14: $r127 = true; break r$127;
                                                          case 15: $r127 = true; break r$127;
                                                          case 16: $r127 = true; break r$127;
                                                          case 17: $r127 = true; break r$127;
                                                          case 18: $r127 = true; break r$127;
                                                          case 19: $r127 = true; break r$127;
                                                          case 20: $r127 = true; break r$127;
                                                          case 21: $r127 = true; break r$127;
                                                          case 22: $r127 = true; break r$127;
                                                          case 23: $r127 = true; break r$127;
                                                          case 24: $r127 = true; break r$127;
                                                          case 25: $r127 = true; break r$127;
                                                          case 26: $r127 = true; break r$127;
                                                          case 27: $r127 = true; break r$127;
                                                          case 28: $r127 = true; break r$127;
                                                          case 29: $r127 = true; break r$127;
                                                          case 30: $r127 = true; break r$127;
                                                          case 31: $r127 = true; break r$127;
                                                          case 32: $r127 = true; break r$127;
                                                          case 33: $r118 = true; break r$118;
                                                          case 34: $r127 = true; break r$127;
                                                          case 35: $r127 = true; break r$127;
                                                          case 36: $r127 = true; break r$127;
                                                          case 37: $r118 = true; break r$118;
                                                          case 38: $r127 = true; break r$127;
                                                          case 39: $r127 = true; break r$127;
                                                          case 40:
                                                            return __
                                                                   (scan_fmt$1130,
                                                                    [ _(add_conv$1125, [ skip$1133, i$1134, conv$1135 ]) ]);
                                                          case 41: $r126 = true; break r$126;
                                                          case 42: $r127 = true; break r$127;
                                                          case 43: $r127 = true; break r$127;
                                                          case 44: $r118 = true; break r$118;
                                                          case 45: $r127 = true; break r$127;
                                                          case 46: $r127 = true; break r$127;
                                                          case 47: $r127 = true; break r$127;
                                                          case 48: $r127 = true; break r$127;
                                                          case 49: $r127 = true; break r$127;
                                                          case 50: $r127 = true; break r$127;
                                                          case 51: $r127 = true; break r$127;
                                                          case 52: $r127 = true; break r$127;
                                                          case 53: $r127 = true; break r$127;
                                                          case 54: $r127 = true; break r$127;
                                                          case 55: $r127 = true; break r$127;
                                                          case 56: $r127 = true; break r$127;
                                                          case 57: $r127 = true; break r$127;
                                                          case 58: $r127 = true; break r$127;
                                                          case 59: $r127 = true; break r$127;
                                                          case 60: $r127 = true; break r$127;
                                                          case 61: $r127 = true; break r$127;
                                                          case 62: $r127 = true; break r$127;
                                                          case 63: $r127 = true; break r$127;
                                                          case 64: $r127 = true; break r$127;
                                                          case 65: $r127 = true; break r$127;
                                                          case 66: $r123 = true; break r$123;
                                                          case 67: $r120 = true; break r$120;
                                                          case 68: $r127 = true; break r$127;
                                                          case 69: $r122 = true; break r$122;
                                                          case 70: $r122 = true; break r$122;
                                                          case 71: $r122 = true; break r$122;
                                                          case 72: $r127 = true; break r$127;
                                                          case 73: $r127 = true; break r$127;
                                                          case 74: $r127 = true; break r$127;
                                                          case 75: $r127 = true; break r$127;
                                                          case 76: $r125 = true; break r$125;
                                                          case 77: $r127 = true; break r$127;
                                                          case 78: $r121 = true; break r$121;
                                                          case 79: $r127 = true; break r$127;
                                                          case 80: $r127 = true; break r$127;
                                                          case 81: $r127 = true; break r$127;
                                                          case 82: $r127 = true; break r$127;
                                                          case 83: $r119 = true; break r$119;
                                                          case 84: $r127 = true; break r$127;
                                                          case 85: $r127 = true; break r$127;
                                                          case 86: $r127 = true; break r$127;
                                                          case 87: $r127 = true; break r$127;
                                                          case 88: $r121 = true; break r$121;
                                                          case 89: $r127 = true; break r$127;
                                                          case 90: $r127 = true; break r$127;
                                                          case 91: $r119 = true; break r$119;
                                                          case 92: $r127 = true; break r$127;
                                                          case 93: $r127 = true; break r$127;
                                                          case 94: $r127 = true; break r$127;
                                                          case 95: $r127 = true; break r$127;
                                                          case 96: $r127 = true; break r$127;
                                                          case 97: $r124 = true; break r$124;
                                                          case 98: $r123 = true; break r$123;
                                                          case 99: $r120 = true; break r$120;
                                                          case 100: $r121 = true; break r$121;
                                                          case 101: $r122 = true; break r$122;
                                                          case 102: $r122 = true; break r$122;
                                                          case 103: $r122 = true; break r$122;
                                                          case 104: $r127 = true; break r$127;
                                                          case 105: $r121 = true; break r$121;
                                                          case 106: $r127 = true; break r$127;
                                                          case 107: $r127 = true; break r$127;
                                                          case 108: $r125 = true; break r$125;
                                                          case 109: $r127 = true; break r$127;
                                                          case 110: $r125 = true; break r$125;
                                                          case 111: $r121 = true; break r$121;
                                                          case 112: $r127 = true; break r$127;
                                                          case 113: $r127 = true; break r$127;
                                                          case 114: $r124 = true; break r$124;
                                                          case 115: $r119 = true; break r$119;
                                                          case 116: $r124 = true; break r$124;
                                                          case 117: $r121 = true; break r$121;
                                                          case 118: $r127 = true; break r$127;
                                                          case 119: $r127 = true; break r$127;
                                                          case 120: $r121 = true; break r$121;
                                                          case 121: $r127 = true; break r$127;
                                                          case 122: $r127 = true; break r$127;
                                                          case 123:
                                                            var i$1143 = _(add_conv$1125, [ skip$1133, i$1134, conv$1135 ]);
                                                            var j$1144 =
                                                              _(sub_format_for_printf$1121, [ conv$1135, fmt$1124, i$1143 ]);
                                                            var loop$1145 =
                                                              _f(function 
                                                                 (i$1146) {
                                                                   if (
                                                                   i$1146 < j$1144 - 2)
                                                                    return __
                                                                    (loop$1145,
                                                                    [ _(add_char$1126, [ i$1146, oc$$srefs(fmt$1124, i$1146) ]) ]);
                                                                   return 0;
                                                                 });
                                                            _(loop$1145, [ i$1143 ]);
                                                            return __(scan_conv$1129, [ skip$1133, j$1144 - 1 ]);
                                                          case 124: $r127 = true; break r$127;
                                                          case 125: $r126 = true; break r$126;
                                                          default: return null;
                                                          }
                                                        }
                                                      }
                                                      if ($r127)
                                                        return __(bad_conversion_format$1061, [ fmt$1124, i$1134, conv$1135 ]);
                                                    }
                                                  }
                                                  if ($r118) return 1 + i$1134;
                                                }
                                              }
                                              if ($r119) return __(add_conv$1125, [ skip$1133, i$1134, 115 ]);
                                            }
                                          }
                                          if ($r120) return __(add_conv$1125, [ skip$1133, i$1134, 99 ]);
                                        }
                                      }
                                      if ($r121) return __(add_conv$1125, [ skip$1133, i$1134, 105 ]);
                                    }
                                  }
                                  if ($r122) return __(add_conv$1125, [ skip$1133, i$1134, 102 ]);
                                }
                              }
                              if ($r123) return __(add_conv$1125, [ skip$1133, i$1134, 66 ]);
                            }
                          }
                          if ($r124) return __(add_conv$1125, [ skip$1133, i$1134, conv$1135 ]);
                        }
                      }
                      if ($r125) {
                        {
                          var j$1141 = 1 + i$1134;
                          if (j$1141 > lim$1127) return __(add_conv$1125, [ skip$1133, i$1134, 105 ]);
                          var c$1142 = oc$$srefs(fmt$1124, j$1141);
                          var $r113 = false;
                          r$113: {
                            {
                              var $r114 = false;
                              r$114: {
                                {
                                  var switcher$1446 = -88 + c$1142;
                                  if (switcher$1446 < 0 || switcher$1446 > 32) { { $r114 = true; break r$114; } }
                                  switch (switcher$1446)
                                  {
                                  case 0: $r113 = true; break r$113;
                                  case 1: $r114 = true; break r$114;
                                  case 2: $r114 = true; break r$114;
                                  case 3: $r114 = true; break r$114;
                                  case 4: $r114 = true; break r$114;
                                  case 5: $r114 = true; break r$114;
                                  case 6: $r114 = true; break r$114;
                                  case 7: $r114 = true; break r$114;
                                  case 8: $r114 = true; break r$114;
                                  case 9: $r114 = true; break r$114;
                                  case 10: $r114 = true; break r$114;
                                  case 11: $r114 = true; break r$114;
                                  case 12: $r113 = true; break r$113;
                                  case 13: $r114 = true; break r$114;
                                  case 14: $r114 = true; break r$114;
                                  case 15: $r114 = true; break r$114;
                                  case 16: $r114 = true; break r$114;
                                  case 17: $r113 = true; break r$113;
                                  case 18: $r114 = true; break r$114;
                                  case 19: $r114 = true; break r$114;
                                  case 20: $r114 = true; break r$114;
                                  case 21: $r114 = true; break r$114;
                                  case 22: $r114 = true; break r$114;
                                  case 23: $r113 = true; break r$113;
                                  case 24: $r114 = true; break r$114;
                                  case 25: $r114 = true; break r$114;
                                  case 26: $r114 = true; break r$114;
                                  case 27: $r114 = true; break r$114;
                                  case 28: $r114 = true; break r$114;
                                  case 29: $r113 = true; break r$113;
                                  case 30: $r114 = true; break r$114;
                                  case 31: $r114 = true; break r$114;
                                  case 32: $r113 = true; break r$113;
                                  default: return null;
                                  }
                                }
                              }
                              if ($r114) return __(add_conv$1125, [ skip$1133, i$1134, 105 ]);
                            }
                          }
                          if ($r113) return __(add_char$1126, [ _(add_conv$1125, [ skip$1133, i$1134, conv$1135 ]), 105 ]);
                        }
                      }
                    }
                  }
                  if ($r126) return __(add_conv$1125, [ skip$1133, i$1134, conv$1135 ]);
                });
           var scan_fmt$1130 =
             _f(function (i$1147) {
                  if (!(i$1147 < lim$1127)) return i$1147;
                  if (oc$$srefs(fmt$1124, i$1147) === 37) return __(scan_fmt$1130, [ _(scan_flags$1128, [ false, 1 + i$1147 ]) ]);
                  return __(scan_fmt$1130, [ 1 + i$1147 ]);
                });
           _(scan_fmt$1130, [ 0 ]);
           return 0;
         });
    var summarize_format_type$1148 =
      _f(function (fmt$1149) {
           var len$1150 = fmt$1149.length;
           var b$1151 = _(oc$Buffer$[0], [ len$1150 ]);
           var add_char$1152 = _f(function (i$1153, c$1154) { _(oc$Buffer$[8], [ b$1151, c$1154 ]); return 1 + i$1153; });
           var add_conv$1155 =
             _f(function (skip$1156, i$1157, c$1158) {
                  if (skip$1156) _(oc$Buffer$[9], [ b$1151, "%_" ]); else _(oc$Buffer$[8], [ b$1151, 37 ]);
                  return __(add_char$1152, [ i$1157, c$1158 ]);
                });
           _(iter_on_format_args$1123, [ fmt$1149, add_conv$1155, add_char$1152 ]);
           return __(oc$Buffer$[1], [ b$1151 ]);
         });
    var Ac$1166 = $();
    var ac_of_format$1170 =
      _f(function (fmt$1171) {
           var ac$1172 = $(0, 0, 0);
           var incr_ac$1173 =
             _f(function (skip$1174, c$1175) {
                  var inc$1176 = c$1175 === 97 ? 2 : 1;
                  if (c$1175 === 114) ac$1172[2] = ac$1172[2] + 1; else;
                  if (skip$1174) return ac$1172[1] = ac$1172[1] + inc$1176;
                  return ac$1172[0] = ac$1172[0] + inc$1176;
                });
           var add_conv$1177 =
             _f(function (skip$1179, i$1180, c$1181) {
                  if (c$1181 !== 41 && c$1181 !== 125) _(incr_ac$1173, [ skip$1179, c$1181 ]); else;
                  return 1 + i$1180;
                });
           var add_char$1178 = _f(function (i$1182, c$1183) { return 1 + i$1182; });
           _(iter_on_format_args$1123, [ fmt$1171, add_conv$1177, add_char$1178 ]);
           return ac$1172;
         });
    var count_arguments_of_format$1184 =
      _f(function (fmt$1185) { var ac$1186 = _(ac_of_format$1170, [ fmt$1185 ]); return ac$1186[0]; });
    var list_iter_i$1187 =
      _f(function (f$1188, l$1189) {
           var loop$1190 =
             _f(function (i$1191, param$1442) {
                  if (param$1442) {
                    {
                      var xs$1194 = param$1442[1];
                      var x$1192 = param$1442[0];
                      if (xs$1194) { { _(f$1188, [ i$1191, x$1192 ]); return __(loop$1190, [ 1 + i$1191, xs$1194 ]); } }
                      return __(f$1188, [ i$1191, x$1192 ]);
                    }
                  }
                  return 0;
                });
           return __(loop$1190, [ 0, l$1189 ]);
         });
    var kapr$1195 =
      _f(function (kpr$1196, fmt$1197) {
           var nargs$1198 = _(count_arguments_of_format$1184, [ fmt$1197 ]);
           if (nargs$1198 < 0 || nargs$1198 > 6) {
             {
               var loop$1226 =
                 _f(function (i$1227, args$1228) {
                      if (i$1227 >= nargs$1198) {
                        {
                          var a$1229 = caml_make_vect(nargs$1198, 0);
                          _(list_iter_i$1187,
                            [
                              _f(function (i$1230, arg$1231) { return oc$$asets(a$1229, nargs$1198 - i$1230 - 1, arg$1231); }),
                              args$1228
                            ]);
                          return __(kpr$1196, [ fmt$1197, a$1229 ]);
                        }
                      }
                      return _f(function (x$1232) { return __(loop$1226, [ 1 + i$1227, $(x$1232, args$1228) ]); });
                    });
               return __(loop$1226, [ 0, 0 ]);
             }
           }
           switch (nargs$1198)
           {
           case 0: return __(kpr$1196, [ fmt$1197, $() ]);
           case 1:
             return _f(function (x$1199) {
                         var a$1200 = caml_make_vect(1, 0);
                         oc$$asets(a$1200, 0, x$1199);
                         return __(kpr$1196, [ fmt$1197, a$1200 ]);
                       });
           case 2:
             return _f(function (x$1201, y$1202) {
                         var a$1203 = caml_make_vect(2, 0);
                         oc$$asets(a$1203, 0, x$1201);
                         oc$$asets(a$1203, 1, y$1202);
                         return __(kpr$1196, [ fmt$1197, a$1203 ]);
                       });
           case 3:
             return _f(function (x$1204, y$1205, z$1206) {
                         var a$1207 = caml_make_vect(3, 0);
                         oc$$asets(a$1207, 0, x$1204);
                         oc$$asets(a$1207, 1, y$1205);
                         oc$$asets(a$1207, 2, z$1206);
                         return __(kpr$1196, [ fmt$1197, a$1207 ]);
                       });
           case 4:
             return _f(function (x$1208, y$1209, z$1210, t$1211) {
                         var a$1212 = caml_make_vect(4, 0);
                         oc$$asets(a$1212, 0, x$1208);
                         oc$$asets(a$1212, 1, y$1209);
                         oc$$asets(a$1212, 2, z$1210);
                         oc$$asets(a$1212, 3, t$1211);
                         return __(kpr$1196, [ fmt$1197, a$1212 ]);
                       });
           case 5:
             return _f(function (x$1213, y$1214, z$1215, t$1216, u$1217) {
                         var a$1218 = caml_make_vect(5, 0);
                         oc$$asets(a$1218, 0, x$1213);
                         oc$$asets(a$1218, 1, y$1214);
                         oc$$asets(a$1218, 2, z$1215);
                         oc$$asets(a$1218, 3, t$1216);
                         oc$$asets(a$1218, 4, u$1217);
                         return __(kpr$1196, [ fmt$1197, a$1218 ]);
                       });
           case 6:
             return _f(function (x$1219, y$1220, z$1221, t$1222, u$1223, v$1224) {
                         var a$1225 = caml_make_vect(6, 0);
                         oc$$asets(a$1225, 0, x$1219);
                         oc$$asets(a$1225, 1, y$1220);
                         oc$$asets(a$1225, 2, z$1221);
                         oc$$asets(a$1225, 3, t$1222);
                         oc$$asets(a$1225, 4, u$1223);
                         oc$$asets(a$1225, 5, v$1224);
                         return __(kpr$1196, [ fmt$1197, a$1225 ]);
                       });
           default: return null;
           }
         });
    var scan_positional_spec$1238 =
      _f(function (fmt$1239, got_spec$1240, n$1241, i$1242) {
           var d$1243 = oc$$srefu(fmt$1239, i$1242);
           if (-48 + d$1243 < 0 || -48 + d$1243 > 9) return __(got_spec$1240, [ 0, i$1242 ]);
           var get_int_literal$1244 =
             _f(function (accu$1245, j$1246) {
                  var d$1247 = oc$$srefu(fmt$1239, j$1246);
                  var $r82 = false;
                  r$82: {
                    {
                      if (!(d$1247 >= 48)) {
                        {
                          if (d$1247 !== 36) { { $r82 = true; break r$82; } }
                          if (accu$1245 === 0) return __(oc$Pervasives$[1], [ "printf: bad positional specification (0)." ]);
                          return __(got_spec$1240, [ $(_(Sformat$1056[3], [ accu$1245 ])), 1 + j$1246 ]);
                        }
                      }
                      if (d$1247 >= 58) { { $r82 = true; break r$82; } }
                      return __(get_int_literal$1244, [ 10 * accu$1245 + (d$1247 - 48), 1 + j$1246 ]);
                    }
                  }
                  if ($r82) return __(got_spec$1240, [ 0, i$1242 ]);
                });
           return __(get_int_literal$1244, [ d$1243 - 48, 1 + i$1242 ]);
         });
    var next_index$1248 =
      _f(function (spec$1249, n$1250) { if (spec$1249) return n$1250; return __(Sformat$1056[2], [ n$1250 ]); });
    var get_index$1251 = _f(function (spec$1252, n$1253) { if (spec$1252) return spec$1252[0]; return n$1253; });
    var format_float_lexeme$1255 =
      function () {
        var make_valid_float_lexeme$1256 =
          _f(function (s$1257) {
               var l$1258 = s$1257.length;
               var valid_float_loop$1259 =
                 _f(function (i$1260) {
                      if (i$1260 >= l$1258) return __(oc$Pervasives$[15], [ s$1257, "." ]);
                      var match$1437 = oc$$srefs(s$1257, i$1260);
                      var $r75 = false;
                      r$75: {
                        {
                          var switcher$1438 = -46 + match$1437;
                          if (!!(switcher$1438 < 0 || switcher$1438 > 23)) {
                            { if (switcher$1438 !== 55) { { $r75 = true; break r$75; } } return s$1257; }
                          }
                          if (!(-1 + switcher$1438 < 0 || -1 + switcher$1438 > 21)) { { $r75 = true; break r$75; } }
                          return s$1257;
                        }
                      }
                      if ($r75) return __(valid_float_loop$1259, [ i$1260 + 1 ]);
                    });
               return __(valid_float_loop$1259, [ 0 ]);
             });
        return _f(function (sfmt$1261, x$1262) {
                    var s$1263 = caml_format_float(sfmt$1261, x$1262);
                    var match$1436 = caml_classify_float(x$1262);
                    if (match$1436 >= 3) return s$1263;
                    return __(make_valid_float_lexeme$1256, [ s$1263 ]);
                  });
      }();
    var scan_format$1264 =
      _f(function (fmt$1265, args$1266, n$1267, pos$1268, cont_s$1269, cont_a$1270, cont_t$1271, cont_f$1272, cont_m$1273) {
           var get_arg$1274 =
             _f(function (spec$1275, n$1276) { return oc$$arefs(args$1266, _(get_index$1251, [ spec$1275, n$1276 ])); });
           var scan_positional$1277 =
             _f(function (n$1280, widths$1281, i$1282) {
                  var got_spec$1283 =
                    _f(function (spec$1284, i$1285) { return __(scan_flags$1278, [ spec$1284, n$1280, widths$1281, i$1285 ]); });
                  return __(scan_positional_spec$1238, [ fmt$1265, got_spec$1283, n$1280, i$1282 ]);
                });
           var scan_flags$1278 =
             _f(function (spec$1286, n$1287, widths$1288, i$1289) {
                  var match$1431 = oc$$srefu(fmt$1265, i$1289);
                  var $r30 = false;
                  r$30: {
                    {
                      var $r31 = false;
                      r$31: {
                        {
                          var switcher$1432 = -32 + match$1431;
                          if (switcher$1432 < 0 || switcher$1432 > 25) { { $r31 = true; break r$31; } }
                          switch (switcher$1432)
                          {
                          case 0: $r30 = true; break r$30;
                          case 1: $r31 = true; break r$31;
                          case 2: $r31 = true; break r$31;
                          case 3: $r30 = true; break r$30;
                          case 4: $r31 = true; break r$31;
                          case 5: $r31 = true; break r$31;
                          case 6: $r31 = true; break r$31;
                          case 7: $r31 = true; break r$31;
                          case 8: $r31 = true; break r$31;
                          case 9: $r31 = true; break r$31;
                          case 10:
                            var got_spec$1290 =
                              _f(function (wspec$1291, i$1292) {
                                   var width$1293 = _(get_arg$1274, [ wspec$1291, n$1287 ]);
                                   return __(scan_flags$1278,
                                             [
                                               spec$1286,
                                               _(next_index$1248, [ wspec$1291, n$1287 ]),
                                               $(width$1293, widths$1288),
                                               i$1292
                                             ]);
                                 });
                            return __(scan_positional_spec$1238, [ fmt$1265, got_spec$1290, n$1287, 1 + i$1289 ]);
                          case 11: $r30 = true; break r$30;
                          case 12: $r31 = true; break r$31;
                          case 13: $r30 = true; break r$30;
                          case 14: $r30 = true; break r$30;
                          case 15: $r31 = true; break r$31;
                          case 16: $r30 = true; break r$30;
                          case 17: $r30 = true; break r$30;
                          case 18: $r30 = true; break r$30;
                          case 19: $r30 = true; break r$30;
                          case 20: $r30 = true; break r$30;
                          case 21: $r30 = true; break r$30;
                          case 22: $r30 = true; break r$30;
                          case 23: $r30 = true; break r$30;
                          case 24: $r30 = true; break r$30;
                          case 25: $r30 = true; break r$30;
                          default: return null;
                          }
                        }
                      }
                      if ($r31) return __(scan_conv$1279, [ spec$1286, n$1287, widths$1288, i$1289 ]);
                    }
                  }
                  if ($r30) return __(scan_flags$1278, [ spec$1286, n$1287, widths$1288, 1 + i$1289 ]);
                });
           var scan_conv$1279 =
             _f(function (spec$1294, n$1295, widths$1296, i$1297) {
                  var conv$1298 = oc$$srefu(fmt$1265, i$1297);
                  var $r67 = false;
                  r$67: {
                    {
                      var $r66 = false;
                      r$66: {
                        {
                          var $r65 = false;
                          r$65: {
                            {
                              var $r64 = false;
                              r$64: {
                                {
                                  var $r63 = false;
                                  r$63: {
                                    {
                                      var $r62 = false;
                                      r$62: {
                                        {
                                          var $r61 = false;
                                          r$61: {
                                            {
                                              var $r68 = false;
                                              r$68: {
                                                {
                                                  if (conv$1298 >= 124) { { $r68 = true; break r$68; } }
                                                  switch (conv$1298)
                                                  {
                                                  case 0: $r68 = true; break r$68;
                                                  case 1: $r68 = true; break r$68;
                                                  case 2: $r68 = true; break r$68;
                                                  case 3: $r68 = true; break r$68;
                                                  case 4: $r68 = true; break r$68;
                                                  case 5: $r68 = true; break r$68;
                                                  case 6: $r68 = true; break r$68;
                                                  case 7: $r68 = true; break r$68;
                                                  case 8: $r68 = true; break r$68;
                                                  case 9: $r68 = true; break r$68;
                                                  case 10: $r68 = true; break r$68;
                                                  case 11: $r68 = true; break r$68;
                                                  case 12: $r68 = true; break r$68;
                                                  case 13: $r68 = true; break r$68;
                                                  case 14: $r68 = true; break r$68;
                                                  case 15: $r68 = true; break r$68;
                                                  case 16: $r68 = true; break r$68;
                                                  case 17: $r68 = true; break r$68;
                                                  case 18: $r68 = true; break r$68;
                                                  case 19: $r68 = true; break r$68;
                                                  case 20: $r68 = true; break r$68;
                                                  case 21: $r68 = true; break r$68;
                                                  case 22: $r68 = true; break r$68;
                                                  case 23: $r68 = true; break r$68;
                                                  case 24: $r68 = true; break r$68;
                                                  case 25: $r68 = true; break r$68;
                                                  case 26: $r68 = true; break r$68;
                                                  case 27: $r68 = true; break r$68;
                                                  case 28: $r68 = true; break r$68;
                                                  case 29: $r68 = true; break r$68;
                                                  case 30: $r68 = true; break r$68;
                                                  case 31: $r68 = true; break r$68;
                                                  case 32: $r68 = true; break r$68;
                                                  case 33: return __(cont_f$1272, [ n$1295, 1 + i$1297 ]);
                                                  case 34: $r68 = true; break r$68;
                                                  case 35: $r68 = true; break r$68;
                                                  case 36: $r68 = true; break r$68;
                                                  case 37: return __(cont_s$1269, [ n$1295, "%", 1 + i$1297 ]);
                                                  case 38: $r68 = true; break r$68;
                                                  case 39: $r68 = true; break r$68;
                                                  case 40: $r67 = true; break r$67;
                                                  case 41: return __(cont_s$1269, [ n$1295, "", 1 + i$1297 ]);
                                                  case 42: $r68 = true; break r$68;
                                                  case 43: $r68 = true; break r$68;
                                                  case 44: return __(cont_s$1269, [ n$1295, "", 1 + i$1297 ]);
                                                  case 45: $r68 = true; break r$68;
                                                  case 46: $r68 = true; break r$68;
                                                  case 47: $r68 = true; break r$68;
                                                  case 48: $r68 = true; break r$68;
                                                  case 49: $r68 = true; break r$68;
                                                  case 50: $r68 = true; break r$68;
                                                  case 51: $r68 = true; break r$68;
                                                  case 52: $r68 = true; break r$68;
                                                  case 53: $r68 = true; break r$68;
                                                  case 54: $r68 = true; break r$68;
                                                  case 55: $r68 = true; break r$68;
                                                  case 56: $r68 = true; break r$68;
                                                  case 57: $r68 = true; break r$68;
                                                  case 58: $r68 = true; break r$68;
                                                  case 59: $r68 = true; break r$68;
                                                  case 60: $r68 = true; break r$68;
                                                  case 61: $r68 = true; break r$68;
                                                  case 62: $r68 = true; break r$68;
                                                  case 63: $r68 = true; break r$68;
                                                  case 64: $r68 = true; break r$68;
                                                  case 65: $r68 = true; break r$68;
                                                  case 66: $r65 = true; break r$65;
                                                  case 67: $r62 = true; break r$62;
                                                  case 68: $r68 = true; break r$68;
                                                  case 69: $r64 = true; break r$64;
                                                  case 70:
                                                    var x$1312 = _(get_arg$1274, [ spec$1294, n$1295 ]);
                                                    var s$1313 =
                                                      widths$1296 === 0 ?
                                                        _(oc$Pervasives$[20], [ x$1312 ]) :
                                                        _(format_float_lexeme$1255,
                                                          [
                                                            _(extract_format$1085, [ fmt$1265, pos$1268, i$1297, widths$1296 ]),
                                                            x$1312
                                                          ]);
                                                    return __(cont_s$1269,
                                                              [ _(next_index$1248, [ spec$1294, n$1295 ]), s$1313, 1 + i$1297 ]);
                                                  case 71: $r64 = true; break r$64;
                                                  case 72: $r68 = true; break r$68;
                                                  case 73: $r68 = true; break r$68;
                                                  case 74: $r68 = true; break r$68;
                                                  case 75: $r68 = true; break r$68;
                                                  case 76: $r66 = true; break r$66;
                                                  case 77: $r68 = true; break r$68;
                                                  case 78: $r63 = true; break r$63;
                                                  case 79: $r68 = true; break r$68;
                                                  case 80: $r68 = true; break r$68;
                                                  case 81: $r68 = true; break r$68;
                                                  case 82: $r68 = true; break r$68;
                                                  case 83: $r61 = true; break r$61;
                                                  case 84: $r68 = true; break r$68;
                                                  case 85: $r68 = true; break r$68;
                                                  case 86: $r68 = true; break r$68;
                                                  case 87: $r68 = true; break r$68;
                                                  case 88: $r63 = true; break r$63;
                                                  case 89: $r68 = true; break r$68;
                                                  case 90: $r68 = true; break r$68;
                                                  case 91: $r68 = true; break r$68;
                                                  case 92: $r68 = true; break r$68;
                                                  case 93: $r68 = true; break r$68;
                                                  case 94: $r68 = true; break r$68;
                                                  case 95: $r68 = true; break r$68;
                                                  case 96: $r68 = true; break r$68;
                                                  case 97:
                                                    var printer$1315 = _(get_arg$1274, [ spec$1294, n$1295 ]);
                                                    var n$1316 = _(Sformat$1056[2], [ _(get_index$1251, [ spec$1294, n$1295 ]) ]);
                                                    var arg$1317 = _(get_arg$1274, [ 0, n$1316 ]);
                                                    return __(cont_a$1270,
                                                              [
                                                                _(next_index$1248, [ spec$1294, n$1316 ]),
                                                                printer$1315,
                                                                arg$1317,
                                                                1 + i$1297
                                                              ]);
                                                  case 98: $r65 = true; break r$65;
                                                  case 99: $r62 = true; break r$62;
                                                  case 100: $r63 = true; break r$63;
                                                  case 101: $r64 = true; break r$64;
                                                  case 102: $r64 = true; break r$64;
                                                  case 103: $r64 = true; break r$64;
                                                  case 104: $r68 = true; break r$68;
                                                  case 105: $r63 = true; break r$63;
                                                  case 106: $r68 = true; break r$68;
                                                  case 107: $r68 = true; break r$68;
                                                  case 108: $r66 = true; break r$66;
                                                  case 109: $r68 = true; break r$68;
                                                  case 110: $r66 = true; break r$66;
                                                  case 111: $r63 = true; break r$63;
                                                  case 112: $r68 = true; break r$68;
                                                  case 113: $r68 = true; break r$68;
                                                  case 114: $r68 = true; break r$68;
                                                  case 115: $r61 = true; break r$61;
                                                  case 116:
                                                    var printer$1318 = _(get_arg$1274, [ spec$1294, n$1295 ]);
                                                    return __(cont_t$1271,
                                                              [
                                                                _(next_index$1248, [ spec$1294, n$1295 ]),
                                                                printer$1318,
                                                                1 + i$1297
                                                              ]);
                                                  case 117: $r63 = true; break r$63;
                                                  case 118: $r68 = true; break r$68;
                                                  case 119: $r68 = true; break r$68;
                                                  case 120: $r63 = true; break r$63;
                                                  case 121: $r68 = true; break r$68;
                                                  case 122: $r68 = true; break r$68;
                                                  case 123: $r67 = true; break r$67;
                                                  default: return null;
                                                  }
                                                }
                                              }
                                              if ($r68) return __(bad_conversion_format$1061, [ fmt$1265, i$1297, conv$1298 ]);
                                            }
                                          }
                                          if ($r61) {
                                            {
                                              var x$1303 = _(get_arg$1274, [ spec$1294, n$1295 ]);
                                              var x$1304 =
                                                conv$1298 === 115 ?
                                                  x$1303 :
                                                  _(oc$Pervasives$[15],
                                                    [ "\"", _(oc$Pervasives$[15], [ _(oc$String$[7], [ x$1303 ]), "\"" ]) ]);
                                              var s$1305 =
                                                i$1297 === 1 + pos$1268 ?
                                                  x$1304 :
                                                  _(format_string$1080,
                                                    [ _(extract_format$1085, [ fmt$1265, pos$1268, i$1297, widths$1296 ]), x$1304 ]);
                                              return __(cont_s$1269,
                                                        [ _(next_index$1248, [ spec$1294, n$1295 ]), s$1305, 1 + i$1297 ]);
                                            }
                                          }
                                        }
                                      }
                                      if ($r62) {
                                        {
                                          var x$1306 = _(get_arg$1274, [ spec$1294, n$1295 ]);
                                          var s$1307 =
                                            conv$1298 === 99 ?
                                              _(oc$String$[0], [ 1, x$1306 ]) :
                                              _(oc$Pervasives$[15],
                                                [ "\'", _(oc$Pervasives$[15], [ _(oc$Char$[1], [ x$1306 ]), "\'" ]) ]);
                                          return __(cont_s$1269, [ _(next_index$1248, [ spec$1294, n$1295 ]), s$1307, 1 + i$1297 ]);
                                        }
                                      }
                                    }
                                  }
                                  if ($r63) {
                                    {
                                      var x$1308 = _(get_arg$1274, [ spec$1294, n$1295 ]);
                                      var s$1309 =
                                        caml_format_int(_(extract_format$1085, [ fmt$1265, pos$1268, i$1297, widths$1296 ]),
                                                        x$1308);
                                      return __(cont_s$1269, [ _(next_index$1248, [ spec$1294, n$1295 ]), s$1309, 1 + i$1297 ]);
                                    }
                                  }
                                }
                              }
                              if ($r64) {
                                {
                                  var x$1310 = _(get_arg$1274, [ spec$1294, n$1295 ]);
                                  var s$1311 =
                                    caml_format_float(_(extract_format$1085, [ fmt$1265, pos$1268, i$1297, widths$1296 ]), x$1310);
                                  return __(cont_s$1269, [ _(next_index$1248, [ spec$1294, n$1295 ]), s$1311, 1 + i$1297 ]);
                                }
                              }
                            }
                          }
                          if ($r65) {
                            {
                              var x$1314 = _(get_arg$1274, [ spec$1294, n$1295 ]);
                              return __(cont_s$1269,
                                        [
                                          _(next_index$1248, [ spec$1294, n$1295 ]),
                                          _(oc$Pervasives$[17], [ x$1314 ]),
                                          1 + i$1297
                                        ]);
                            }
                          }
                        }
                      }
                      if ($r66) {
                        {
                          var match$1434 = oc$$srefu(fmt$1265, 1 + i$1297);
                          var $r56 = false;
                          r$56: {
                            {
                              var $r57 = false;
                              r$57: {
                                {
                                  var switcher$1435 = -88 + match$1434;
                                  if (switcher$1435 < 0 || switcher$1435 > 32) { { $r57 = true; break r$57; } }
                                  switch (switcher$1435)
                                  {
                                  case 0: $r56 = true; break r$56;
                                  case 1: $r57 = true; break r$57;
                                  case 2: $r57 = true; break r$57;
                                  case 3: $r57 = true; break r$57;
                                  case 4: $r57 = true; break r$57;
                                  case 5: $r57 = true; break r$57;
                                  case 6: $r57 = true; break r$57;
                                  case 7: $r57 = true; break r$57;
                                  case 8: $r57 = true; break r$57;
                                  case 9: $r57 = true; break r$57;
                                  case 10: $r57 = true; break r$57;
                                  case 11: $r57 = true; break r$57;
                                  case 12: $r56 = true; break r$56;
                                  case 13: $r57 = true; break r$57;
                                  case 14: $r57 = true; break r$57;
                                  case 15: $r57 = true; break r$57;
                                  case 16: $r57 = true; break r$57;
                                  case 17: $r56 = true; break r$56;
                                  case 18: $r57 = true; break r$57;
                                  case 19: $r57 = true; break r$57;
                                  case 20: $r57 = true; break r$57;
                                  case 21: $r57 = true; break r$57;
                                  case 22: $r57 = true; break r$57;
                                  case 23: $r56 = true; break r$56;
                                  case 24: $r57 = true; break r$57;
                                  case 25: $r57 = true; break r$57;
                                  case 26: $r57 = true; break r$57;
                                  case 27: $r57 = true; break r$57;
                                  case 28: $r57 = true; break r$57;
                                  case 29: $r56 = true; break r$56;
                                  case 30: $r57 = true; break r$57;
                                  case 31: $r57 = true; break r$57;
                                  case 32: $r56 = true; break r$56;
                                  default: return null;
                                  }
                                }
                              }
                              if ($r57) {
                                {
                                  var x$1324 = _(get_arg$1274, [ spec$1294, n$1295 ]);
                                  var s$1325 =
                                    caml_format_int(_(extract_format$1085, [ fmt$1265, pos$1268, i$1297, widths$1296 ]), x$1324);
                                  return __(cont_s$1269, [ _(next_index$1248, [ spec$1294, n$1295 ]), s$1325, 1 + i$1297 ]);
                                }
                              }
                            }
                          }
                          if ($r56) {
                            {
                              var i$1319 = 1 + i$1297;
                              var s$1320 =
                                function () {
                                  var $r51 = false;
                                  r$51: {
                                    {
                                      var switcher$1433 = -108 + conv$1298;
                                      if (switcher$1433 < 0 || switcher$1433 > 2) { { $r51 = true; break r$51; } }
                                      switch (switcher$1433)
                                      {
                                      case 0:
                                        var x$1321 = _(get_arg$1274, [ spec$1294, n$1295 ]);
                                        return caml_format_int(_(extract_format$1085, [ fmt$1265, pos$1268, i$1319, widths$1296 ]),
                                                               x$1321);
                                      case 1: $r51 = true; break r$51;
                                      case 2:
                                        var x$1322 = _(get_arg$1274, [ spec$1294, n$1295 ]);
                                        return caml_format_int(_(extract_format$1085, [ fmt$1265, pos$1268, i$1319, widths$1296 ]),
                                                               x$1322);
                                      default: return null;
                                      }
                                    }
                                  }
                                  if ($r51) {
                                    {
                                      var x$1323 = _(get_arg$1274, [ spec$1294, n$1295 ]);
                                      return caml_format_int(_(extract_format$1085, [ fmt$1265, pos$1268, i$1319, widths$1296 ]),
                                                             x$1323);
                                    }
                                  }
                                }();
                              return __(cont_s$1269, [ _(next_index$1248, [ spec$1294, n$1295 ]), s$1320, 1 + i$1319 ]);
                            }
                          }
                        }
                      }
                    }
                  }
                  if ($r67) {
                    {
                      var xf$1326 = _(get_arg$1274, [ spec$1294, n$1295 ]);
                      var i$1327 = 1 + i$1297;
                      var j$1328 = _(sub_format_for_printf$1121, [ conv$1298, fmt$1265, i$1327 ]);
                      if (conv$1298 === 123)
                        return __(cont_s$1269,
                                  [ _(next_index$1248, [ spec$1294, n$1295 ]), _(summarize_format_type$1148, [ xf$1326 ]), j$1328 ]);
                      return __(cont_m$1273, [ _(next_index$1248, [ spec$1294, n$1295 ]), xf$1326, j$1328 ]);
                    }
                  }
                });
           return __(scan_positional$1277, [ n$1267, 0, 1 + pos$1268 ]);
         });
    var mkprintf$1329 =
      _f(function (to_s$1330, get_out$1331, outc$1332, outs$1333, flush$1334, k$1335, fmt$1336) {
           var out$1337 = _(get_out$1331, [ fmt$1336 ]);
           var pr$1338 =
             _f(function (k$1339, n$1340, fmt$1341, v$1342) {
                  var len$1343 = fmt$1341.length;
                  var doprn$1344 =
                    _f(function (n$1350, i$1351) {
                         if (i$1351 >= len$1343) return _(k$1339, [ out$1337 ]);
                         var c$1352 = oc$$srefu(fmt$1341, i$1351);
                         if (c$1352 !== 37) {
                           { _(outc$1332, [ out$1337, c$1352 ]); return __(doprn$1344, [ n$1350, 1 + i$1351 ]); }
                         }
                         return __(scan_format$1264,
                                   [
                                     fmt$1341,
                                     v$1342,
                                     n$1350,
                                     i$1351,
                                     cont_s$1345,
                                     cont_a$1346,
                                     cont_t$1347,
                                     cont_f$1348,
                                     cont_m$1349
                                   ]);
                       });
                  var cont_s$1345 =
                    _f(function (n$1353, s$1354, i$1355) {
                         _(outs$1333, [ out$1337, s$1354 ]);
                         return __(doprn$1344, [ n$1353, i$1355 ]);
                       });
                  var cont_a$1346 =
                    _f(function (n$1356, printer$1357, arg$1358, i$1359) {
                         if (to_s$1330)
                           _(outs$1333, [ out$1337, _(printer$1357, [ 0, arg$1358 ]) ]);
                         else
                           _(printer$1357, [ out$1337, arg$1358 ]);
                         return __(doprn$1344, [ n$1356, i$1359 ]);
                       });
                  var cont_t$1347 =
                    _f(function (n$1360, printer$1361, i$1362) {
                         if (to_s$1330) _(outs$1333, [ out$1337, _(printer$1361, [ 0 ]) ]); else _(printer$1361, [ out$1337 ]);
                         return __(doprn$1344, [ n$1360, i$1362 ]);
                       });
                  var cont_f$1348 =
                    _f(function (n$1363, i$1364) { _(flush$1334, [ out$1337 ]); return __(doprn$1344, [ n$1363, i$1364 ]); });
                  var cont_m$1349 =
                    _f(function (n$1365, xf$1366, i$1367) {
                         var m$1368 = _(Sformat$1056[1], [ _(count_arguments_of_format$1184, [ xf$1366 ]), n$1365 ]);
                         return __(pr$1338,
                                   [
                                     _f(function (param$1430) { return __(doprn$1344, [ m$1368, i$1367 ]); }),
                                     n$1365,
                                     xf$1366,
                                     v$1342
                                   ]);
                       });
                  return __(doprn$1344, [ n$1340, 0 ]);
                });
           var kpr$1369 = _(pr$1338, [ k$1335, _(Sformat$1056[0], [ 0 ]) ]);
           return __(kapr$1195, [ kpr$1369, fmt$1336 ]);
         });
    var kfprintf$1370 =
      _f(function (k$1371, oc$1372) {
           return __(mkprintf$1329,
                     [
                       false,
                       _f(function (param$1429) { return oc$1372; }),
                       oc$Pervasives$[45],
                       oc$Pervasives$[46],
                       oc$Pervasives$[43],
                       k$1371
                     ]);
         });
    var ifprintf$1373 =
      _f(function (oc$1374) {
           return __(kapr$1195, [ _f(function (param$1427) { return _f(function (prim$1428) { return 0; }); }) ]);
         });
    var fprintf$1375 = _f(function (oc$1376) { return __(kfprintf$1370, [ _f(function (prim$1426) { return 0; }), oc$1376 ]); });
    var printf$1377 = _f(function (fmt$1378) { return __(fprintf$1375, [ oc$Pervasives$[23], fmt$1378 ]); });
    var eprintf$1379 = _f(function (fmt$1380) { return __(fprintf$1375, [ oc$Pervasives$[24], fmt$1380 ]); });
    var kbprintf$1381 =
      _f(function (k$1382, b$1383) {
           return __(mkprintf$1329,
                     [
                       false,
                       _f(function (param$1424) { return b$1383; }),
                       oc$Buffer$[8],
                       oc$Buffer$[9],
                       _f(function (prim$1425) { return 0; }),
                       k$1382
                     ]);
         });
    var bprintf$1384 = _f(function (b$1385) { return __(kbprintf$1381, [ _f(function (prim$1423) { return 0; }), b$1385 ]); });
    var get_buff$1386 = _f(function (fmt$1387) { var len$1388 = 2 * fmt$1387.length; return __(oc$Buffer$[0], [ len$1388 ]); });
    var get_contents$1389 =
      _f(function (b$1390) { var s$1391 = _(oc$Buffer$[1], [ b$1390 ]); _(oc$Buffer$[6], [ b$1390 ]); return s$1391; });
    var get_cont$1392 = _f(function (k$1393, b$1394) { return __(k$1393, [ _(get_contents$1389, [ b$1394 ]) ]); });
    var ksprintf$1395 =
      _f(function (k$1396) {
           return __(mkprintf$1329,
                     [
                       true,
                       get_buff$1386,
                       oc$Buffer$[8],
                       oc$Buffer$[9],
                       _f(function (prim$1422) { return 0; }),
                       _(get_cont$1392, [ k$1396 ])
                     ]);
         });
    var sprintf$1398 =
      _f(function (fmt$1399) { return __(ksprintf$1395, [ _f(function (s$1400) { return s$1400; }), fmt$1399 ]); });
    var CamlinternalPr$1415 =
      function () {
        var Tformat$1414 = $(ac_of_format$1170, sub_format$1103, summarize_format_type$1148, scan_format$1264, kapr$1195);
        return $(Sformat$1056, Tformat$1414);
      }();
    return $(fprintf$1375, printf$1377, eprintf$1379, ifprintf$1373, sprintf$1398, bprintf$1384, kfprintf$1370, ksprintf$1395,
             kbprintf$1381, ksprintf$1395,
             $(function () { var let$1421 = CamlinternalPr$1415[0]; return $(let$1421[0], let$1421[2], let$1421[4], let$1421[5]); }
               (), CamlinternalPr$1415[1]));
  }();
var oc$Random$ =
  function () {
    var init$1046 = _f(function (prim$1096) { return 0; });
    var full_init$1047 = _f(function (prim$1095) { return 0; });
    var self_init$1048 = _f(function (prim$1094) { return 0; });
    var bits$1049 = _f(function (param$1093) { return Math.floor(Math.random() * 1073741824); });
    var int$1050 = _f(function (b$1051) { return Math.floor(Math.random() * b$1051); });
    var int32$1052 = _f(function (b$1053) { return Math.floor(Math.random() * b$1053); });
    var nativeint$1054 = _f(function (b$1055) { return Math.floor(Math.random() * b$1055); });
    var int64$1056 = _f(function (param$1092) { return oc$Int64$[0]; });
    var float$1057 = _f(function (b$1058) { return Math.random() * b$1058; });
    var bool$1059 = _f(function (param$1091) { return Math.random() < 0.5; });
    var State$1076 =
      function () {
        var make$1061 = _f(function (prim$1090) { return 0; });
        var make_self_init$1062 = _f(function (prim$1089) { return 0; });
        var copy$1063 = _f(function (prim$1088) { return 0; });
        var bits$1064 = _f(function (param$1087) { return __(bits$1049, [ 0 ]); });
        var int$1065 = _f(function (param$1086, b$1066) { return __(int$1050, [ b$1066 ]); });
        var int32$1067 = _f(function (param$1085, b$1068) { return __(int32$1052, [ b$1068 ]); });
        var nativeint$1069 = _f(function (param$1084, b$1070) { return __(nativeint$1054, [ b$1070 ]); });
        var int64$1071 = _f(function (param$1083, b$1072) { return __(int64$1056, [ b$1072 ]); });
        var float$1073 = _f(function (param$1082, b$1074) { return __(float$1057, [ b$1074 ]); });
        var bool$1075 = _f(function (param$1081) { return __(bool$1059, [ 0 ]); });
        return $(make$1061, make_self_init$1062, copy$1063, bits$1064, int$1065, int32$1067, nativeint$1069, int64$1071,
                 float$1073, bool$1075);
      }();
    var get_state$1077 = _f(function (prim$1080) { return 0; });
    var set_state$1078 = _f(function (prim$1079) { return 0; });
    return $(init$1046, full_init$1047, self_init$1048, bits$1049, int$1050, int32$1052, nativeint$1054, int64$1056, float$1057,
             bool$1059, State$1076, get_state$1077, set_state$1078);
  }();
var oc$ListLabels$ =
  function () {
    var include$1071 = oc$List$;
    return $(include$1071[0], include$1071[1], include$1071[2], include$1071[3], 
             include$1071[4], include$1071[5], include$1071[6], include$1071[7], 
             include$1071[8], include$1071[9], include$1071[10], include$1071[11], 
             include$1071[12], include$1071[13], include$1071[14], include$1071[15], 
             include$1071[16], include$1071[17], include$1071[18], include$1071[19], 
             include$1071[20], include$1071[21], include$1071[22], include$1071[23], 
             include$1071[24], include$1071[25], include$1071[26], include$1071[27], 
             include$1071[28], include$1071[29], include$1071[30], include$1071[31], 
             include$1071[32], include$1071[33], include$1071[34], include$1071[35], 
             include$1071[36], include$1071[37], include$1071[38], include$1071[39], 
             include$1071[40]);
  }();
var oc$Froc_dlist$ =
  function () {
    var empty$1043 =
      _f(function (param$1063) { var t$1044 = $(0, t$1044, t$1044); t$1044[1] = t$1044; t$1044[2] = t$1044; return t$1044; });
    var add_after$1045 =
      _f(function (t$1046, d$1047) {
           var n$1048 = $(d$1047, t$1046, t$1046[2]);
           t$1046[2][1] = n$1048;
           t$1046[2] = n$1048;
           return n$1048;
         });
    var remove$1049 =
      _f(function (t$1050) { t$1050[2][1] = t$1050[1]; t$1050[1][2] = t$1050[2]; t$1050[2] = t$1050; return t$1050[1] = t$1050; });
    var iter$1051 =
      _f(function (f$1052, d$1053) {
           var loop$1054 =
             _f(function (t$1055) {
                  if (!(t$1055 === d$1053)) {
                    { var next$1056 = t$1055[2]; _(f$1052, [ t$1055[0] ]); return __(loop$1054, [ next$1056 ]); }
                  }
                  return 0;
                });
           return __(loop$1054, [ d$1053[2] ]);
         });
    var clear$1057 = _f(function (d$1058) { d$1058[2] = d$1058; return d$1058[1] = d$1058; });
    return $(empty$1043, add_after$1045, remove$1049, iter$1051, clear$1057);
  }();
var oc$Froc_hashtbl$ =
  function () {
    var total_eq$1056 =
      _f(function (v1$1057, v2$1058) { try { return caml_compare(v1$1057, v2$1058) === 0; } catch (exn$1146) { return false; } });
    var create$1059 =
      _f(function ($2Aopt$2A$1060, $2Aopt$2A$1063, $2Aopt$2A$1083, param$1144) {
           var size$1061 = $2Aopt$2A$1060 ? $2Aopt$2A$1060[0] : 17;
           var hash$1064 = $2Aopt$2A$1063 ? $2Aopt$2A$1063[0] : oc$Hashtbl$[13];
           var eq$1084 = $2Aopt$2A$1083 ? $2Aopt$2A$1083[0] : total_eq$1056;
           var s$1086 = _(oc$Pervasives$[3], [ _(oc$Pervasives$[4], [ 1, size$1061 ]), oc$Sys$[6] ]);
           return $(hash$1064, eq$1084, 0, caml_make_vect(s$1086, 0));
         });
    var resize$1087 =
      _f(function (tbl$1088) {
           var odata$1089 = tbl$1088[3];
           var osize$1090 = odata$1089.length;
           var nsize$1091 = _(oc$Pervasives$[3], [ 2 * osize$1090 + 1, oc$Sys$[6] ]);
           if (nsize$1091 !== osize$1090) {
             {
               var ndata$1092 = caml_make_vect(nsize$1091, 0);
               var insert_bucket$1093 =
                 _f(function (param$1143) {
                      if (param$1143) {
                        {
                          var key$1094 = param$1143[0];
                          _(insert_bucket$1093, [ param$1143[2] ]);
                          var nidx$1097 = _(tbl$1088[0], [ key$1094 ]) % nsize$1091;
                          return oc$$asets(ndata$1092, nidx$1097, $(key$1094, param$1143[1], oc$$arefs(ndata$1092, nidx$1097)));
                        }
                      }
                      return 0;
                    });
               for (var i$1098 = 0; i$1098 <= osize$1090 - 1; i$1098++) {
                 (function (i$1098) { _(insert_bucket$1093, [ oc$$arefs(odata$1089, i$1098) ]); }(i$1098));
               }
               return tbl$1088[3] = ndata$1092;
             }
           }
           return 0;
         });
    var add$1099 =
      _f(function (h$1100, key$1101, info$1102) {
           var i$1103 = _(h$1100[0], [ key$1101 ]) % (h$1100[3]).length;
           var bucket$1104 = $(key$1101, info$1102, oc$$arefs(h$1100[3], i$1103));
           oc$$asets(h$1100[3], i$1103, bucket$1104);
           h$1100[2] = 1 + h$1100[2];
           if (h$1100[2] > (h$1100[3]).length << 1) return __(resize$1087, [ h$1100 ]);
           return 0;
         });
    var remove$1105 =
      _f(function (h$1106, key$1107, p$1108) {
           var remove_bucket$1109 =
             _f(function (param$1142) {
                  if (param$1142) {
                    {
                      var next$1112 = param$1142[2];
                      var i$1111 = param$1142[1];
                      var k$1110 = param$1142[0];
                      if (_(h$1106[1], [ k$1110, key$1107 ]) && _(p$1108, [ i$1111 ])) {
                        { h$1106[2] = -1 + h$1106[2]; return next$1112; }
                      }
                      return $(k$1110, i$1111, _(remove_bucket$1109, [ next$1112 ]));
                    }
                  }
                  return 0;
                });
           var i$1113 = _(h$1106[0], [ key$1107 ]) % (h$1106[3]).length;
           return oc$$asets(h$1106[3], i$1113, _(remove_bucket$1109, [ oc$$arefs(h$1106[3], i$1113) ]));
         });
    var find_rec$1114 =
      _f(function (h$1115, key$1116, p$1117, param$1141) {
           if (param$1141) {
             {
               var d$1119 = param$1141[1];
               if (_(h$1115[1], [ key$1116, param$1141[0] ]) && _(p$1117, [ d$1119 ])) return d$1119;
               return __(find_rec$1114, [ h$1115, key$1116, p$1117, param$1141[2] ]);
             }
           }
           throw $(Not_found$20g);
         });
    var find$1121 =
      _f(function (h$1122, key$1123, p$1124) {
           var match$1140 = oc$$arefs(h$1122[3], _(h$1122[0], [ key$1123 ]) % (h$1122[3]).length);
           if (match$1140) {
             {
               var rest1$1127 = match$1140[2];
               var d1$1126 = match$1140[1];
               if (_(h$1122[1], [ key$1123, match$1140[0] ]) && _(p$1124, [ d1$1126 ])) return d1$1126;
               if (rest1$1127) {
                 {
                   var rest2$1130 = rest1$1127[2];
                   var d2$1129 = rest1$1127[1];
                   if (_(h$1122[1], [ key$1123, rest1$1127[0] ]) && _(p$1124, [ d2$1129 ])) return d2$1129;
                   if (rest2$1130) {
                     {
                       var d3$1132 = rest2$1130[1];
                       if (_(h$1122[1], [ key$1123, rest2$1130[0] ]) && _(p$1124, [ d3$1132 ])) return d3$1132;
                       return __(find_rec$1114, [ h$1122, key$1123, p$1124, rest2$1130[2] ]);
                     }
                   }
                   throw $(Not_found$20g);
                 }
               }
               throw $(Not_found$20g);
             }
           }
           throw $(Not_found$20g);
         });
    return $(create$1059, add$1099, find$1121, remove$1105);
  }();
var oc$Froc_timestamp$ =
  function () {
    var debug$1030 = $(_f(function (prim$1132) { return 0; }));
    var set_debug$1031 = _f(function (f$1032) { return debug$1030[0] = f$1032; });
    var is_spliced_out$1044 = _f(function (t$1045) { return t$1045[1]; });
    var check$1046 = _f(function (t$1047) { if (t$1047[1]) throw $(Invalid_argument$18g, "spliced out timestamp"); return 0; });
    var empty$1048 =
      _f(function (param$1129) {
           var h$1049 = $(0, false, t$1050, h$1049, 0);
           var t$1050 = $(oc$Pervasives$[6], false, t$1050, h$1049, 0);
           h$1049[2] = t$1050;
           h$1049[3] = h$1049;
           t$1050[2] = t$1050;
           t$1050[3] = h$1049;
           return h$1049;
         });
    var timeline$1051 = $(_(empty$1048, [ 0 ]));
    var now$1052 = $(timeline$1051[0]);
    var get_now$1053 = _f(function (param$1128) { return now$1052[0]; });
    var set_now$1054 = _f(function (t$1055) { return now$1052[0] = t$1055; });
    var init$1056 =
      _f(function (param$1127) {
           var loop$1057 =
             _f(function (t$1058) {
                  if (t$1058 !== t$1058[2]) {
                    {
                      _(oc$List$[9], [ _f(function (c$1059) { return __(c$1059, [ 0 ]); }), t$1058[4] ]);
                      return __(loop$1057, [ t$1058[2] ]);
                    }
                  }
                  return 0;
                });
           _(loop$1057, [ timeline$1051[0] ]);
           timeline$1051[0] = _(empty$1048, [ 0 ]);
           return now$1052[0] = timeline$1051[0];
         });
    var tau_factor$1060 = 1.41421;
    var renumber$1061 =
      _f(function (t$1062) {
           var find_range$1063 =
             _f(function (lo$1064, hi$1065, mask$1066, tau$1067, count$1068) {
                  var lo_id$1069 = lo$1064[0] & _(oc$Pervasives$[8], [ mask$1066 ]);
                  var hi_id$1070 = lo_id$1069 | mask$1066;
                  var lo_loop$1071 =
                    _f(function (lo$1072, count$1073) {
                         var lo_prev$1074 = lo$1072[3];
                         if (lo_prev$1074[0] < lo_id$1069 || lo_prev$1074[3] === lo_prev$1074) return $(lo$1072, count$1073);
                         return __(lo_loop$1071, [ lo_prev$1074, count$1073 + 1 ]);
                       });
                  var hi_loop$1075 =
                    _f(function (hi$1076, count$1077) {
                         var hi_next$1078 = hi$1076[2];
                         if (hi_next$1078[0] > hi_id$1070 || hi_next$1078[2] === hi_next$1078) return $(hi$1076, count$1077);
                         return __(hi_loop$1075, [ hi_next$1078, count$1077 + 1 ]);
                       });
                  var match$1126 = _(lo_loop$1071, [ lo$1064, count$1068 ]);
                  var lo$1079 = match$1126[0];
                  var match$1125 = _(hi_loop$1075, [ hi$1065, match$1126[1] ]);
                  var count$1082 = match$1125[1];
                  var hi$1081 = match$1125[0];
                  var size$1083 = mask$1066 + 1;
                  var density$1084 = count$1082 / size$1083;
                  if (density$1084 < tau$1067) return $(lo$1079, hi$1081, lo_id$1069, count$1082, size$1083);
                  var mask$1085 = mask$1066 * 2 + 1;
                  if (mask$1085 === oc$Pervasives$[6]) _(oc$Pervasives$[1], [ "out of timestamps" ]); else;
                  return __(find_range$1063, [ lo$1079, hi$1081, mask$1085, tau$1067 / tau_factor$1060, count$1082 ]);
                });
           var match$1124 = _(find_range$1063, [ t$1062, t$1062, 1, 1. / tau_factor$1060, 1 ]);
           var incr$1091 = match$1124[4] / match$1124[3] >> 0;
           var ren_loop$1092 =
             _f(function (t$1093, id$1094) {
                  t$1093[0] = id$1094;
                  if (t$1093 !== match$1124[1]) return __(ren_loop$1092, [ t$1093[2], id$1094 + incr$1091 ]);
                  return 0;
                });
           return __(ren_loop$1092, [ match$1124[0], match$1124[2] ]);
         });
    var tick$1095 =
      _f(function (param$1122) {
           var t$1096 = now$1052[0];
           _(check$1046, [ t$1096 ]);
           var next_id$1097 = t$1096[2][0];
           var id$1098 =
             function () {
               var incr$1099 = Math.sqrt(next_id$1097 - t$1096[0]) >> 0;
               var id$1100 = t$1096[0] + incr$1099;
               if (id$1100 === next_id$1097) return t$1096[0];
               return id$1100;
             }();
           var t$27$1101 = $(id$1098, false, t$1096[2], t$1096, 0);
           t$1096[2][3] = t$27$1101;
           t$1096[2] = t$27$1101;
           if (id$1098 === t$1096[0]) _(renumber$1061, [ t$1096 ]); else;
           now$1052[0] = t$27$1101;
           return t$27$1101;
         });
    var add_cleanup$1102 =
      _f(function (t$1103, cleanup$1104) { _(check$1046, [ t$1103 ]); return t$1103[4] = $(cleanup$1104, t$1103[4]); });
    var splice_out$1105 =
      _f(function (t1$1106, t2$1107) {
           _(check$1046, [ t1$1106 ]);
           _(check$1046, [ t2$1107 ]);
           if (t1$1106[0] >= t2$1107[0]) throw $(Invalid_argument$18g, "t1 >= t2"); else;
           var loop$1108 =
             _f(function (t$1109) {
                  if (t$1109 === t2$1107) return 0;
                  _(oc$List$[9], [ _f(function (c$1110) { return __(c$1110, [ 0 ]); }), t$1109[4] ]);
                  t$1109[4] = 0;
                  t$1109[1] = true;
                  return __(loop$1108, [ t$1109[2] ]);
                });
           _(loop$1108, [ t1$1106[2] ]);
           t1$1106[2] = t2$1107;
           return t2$1107[3] = t1$1106;
         });
    var compare$1111 =
      _f(function (t1$1112, t2$1113) {
           _(check$1046, [ t1$1112 ]);
           _(check$1046, [ t2$1113 ]);
           return caml_int_compare(t1$1112[0], t2$1113[0]);
         });
    var eq$1114 =
      _f(function (t1$1115, t2$1116) { _(check$1046, [ t1$1115 ]); _(check$1046, [ t2$1116 ]); return t1$1115 === t2$1116; });
    return $(init$1056, tick$1095, get_now$1053, set_now$1054, add_cleanup$1102, splice_out$1105, is_spliced_out$1044,
             compare$1111, eq$1114, set_debug$1031);
  }();
var oc$Froc_ddg$ =
  function () {
    var Dlist$1046 = oc$Froc_dlist$;
    var TS$1047 = oc$Froc_timestamp$;
    var debug$1048 = $(_f(function (prim$2187) { return 0; }));
    var set_debug$1049 = _f(function (f$1050) { debug$1048[0] = f$1050; return __(TS$1047[9], [ f$1050 ]); });
    var handle_exn$1051 = $(_f(function (prim$2186) { throw prim$2186; }));
    var set_exn_handler$1052 = _f(function (h$1053) { return handle_exn$1051[0] = h$1053; });
    var total_eq$1087 =
      _f(function (v1$1088, v2$1089) { try { return caml_compare(v1$1088, v2$1089) === 0; } catch (exn$2185) { return false; } });
    var hash$1090 =
      _f(function (t$1091) {
           var match$2183 = t$1091;
           switch ($t(match$2183)) { case 0: return match$2183[0]; case 1: return match$2183[0][0]; default: return null; }
         });
    var Unset$1094 = $("Froc_ddg.Unset");
    var next_id$1095 =
      function () {
        var next_id$1096 = $(1);
        return _f(function (param$2182) { var id$1097 = next_id$1096[0]; next_id$1096[0]++; return id$1097; });
      }();
    var unset$1098 = $1($(Unset$1094));
    var make_changeable$1099 =
      _f(function ($2Aopt$2A$1100, $2Aopt$2A$1103, param$2180) {
           var eq$1101 = $2Aopt$2A$1100 ? $2Aopt$2A$1100[0] : total_eq$1087;
           var result$1104 = $2Aopt$2A$1103 ? $2Aopt$2A$1103[0] : unset$1098;
           var c$1106 = $(_(next_id$1095, [ 0 ]), eq$1101, result$1104, _(Dlist$1046[0], [ 0 ]));
           return $($1(c$1106), c$1106);
         });
    var make_constant$1107 = _f(function (result$1108) { return $(_(next_id$1095, [ 0 ]), result$1108); });
    var changeable$1109 = _f(function (eq$1110, v$1111) { return __(make_changeable$1099, [ eq$1110, $($(v$1111)), 0 ]); });
    var return$1112 = _f(function (v$1113) { return __(make_constant$1107, [ $(v$1113) ]); });
    var fail$1114 = _f(function (e$1115) { return __(make_constant$1107, [ $1(e$1115) ]); });
    var is_constant$1116 =
      _f(function (t$1117) { var match$2177 = t$1117; switch ($t(match$2177)) { case 0: return true; default: return false; } });
    var clear$1118 = _f(function (u$1119) { var c$1120 = u$1119; return c$1120[2] = unset$1098; });
    var write_result$1121 =
      _f(function (u$1122, r$1123) {
           var c$1124 = u$1122;
           var eq$1125 =
             function () {
               var match$2174 = c$1124[2];
               var $r254 = false;
               r$254:
                 switch ($t(match$2174))
                 {
                 case 0:
                   switch ($t(r$1123))
                   {
                   case 0: return _(c$1124[1], [ match$2174[0], r$1123[0] ]);
                   default: $r254 = true; break r$254;
                   }
                   break;
                 case 1:
                   switch ($t(r$1123)) { case 1: return match$2174[0] === r$1123[0]; default: $r254 = true; break r$254; }
                   break;
                 default: return null;
                 }
               if ($r254) return false;
             }();
           if (!eq$1125) {
             {
               c$1124[2] = r$1123;
               return __(Dlist$1046[3], [ _f(function (f$1130) { return __(f$1130, [ r$1123 ]); }), c$1124[3] ]);
             }
           }
           return 0;
         });
    var write_result_no_eq$1131 =
      _f(function (u$1132, r$1133) {
           var c$1134 = u$1132;
           c$1134[2] = r$1133;
           return __(Dlist$1046[3], [ _f(function (f$1135) { return __(f$1135, [ r$1133 ]); }), c$1134[3] ]);
         });
    var write$1136 = _f(function (u$1137, v$1138) { return __(write_result$1121, [ u$1137, $(v$1138) ]); });
    var write_exn$1139 = _f(function (u$1140, e$1141) { return __(write_result$1121, [ u$1140, $1(e$1141) ]); });
    var read_result$1142 =
      _f(function (t$1143) {
           var match$2172 = t$1143;
           switch ($t(match$2172)) { case 0: return match$2172[1]; case 1: return match$2172[0][2]; default: return null; }
         });
    var read$1146 =
      _f(function (t$1147) {
           var match$2171 = _(read_result$1142, [ t$1147 ]);
           switch ($t(match$2171)) { case 0: return match$2171[0]; case 1: throw match$2171[0]; default: return null; }
         });
    var make_cancel$1151 = _f(function (f$1152) { return f$1152; });
    var no_cancel$1153 = _f(function (prim$2170) { return 0; });
    var cancel$1154 = _f(function (c$1155) { return __(c$1155, [ 0 ]); });
    var add_dep_cancel$1156 =
      _f(function (ts$1157, t$1158, dep$1159) {
           var cancel$1160 =
             function () {
               var match$2167 = t$1158;
               switch ($t(match$2167))
               {
               case 0: return no_cancel$1153;
               case 1:
                 var dl$1162 = _(Dlist$1046[1], [ match$2167[0][3], dep$1159 ]);
                 return _(make_cancel$1151, [ _f(function (param$2166) { return __(Dlist$1046[2], [ dl$1162 ]); }) ]);
               default: return null;
               }
             }();
           _(TS$1047[4], [ ts$1157, cancel$1160 ]);
           return cancel$1160;
         });
    var add_dep$1163 =
      _f(function (ts$1164, t$1165, dep$1166) { var match$2165 = _(add_dep_cancel$1156, [ ts$1164, t$1165, dep$1166 ]); return 0; });
    var PQ$1235 =
      function () {
        var make$1179 = _f(function (param$2163) { return $($(), 0); });
        var is_empty$1180 = _f(function (t$1181) { return t$1181[1] === 0; });
        var size$1182 = _f(function (t$1183) { return t$1183[1]; });
        var compare_down$1184 =
          _f(function (h$1185, i$1186, i$27$1187) {
               var t1$1188 = h$1185[0][i$1186][1];
               var t2$1189 = h$1185[0][i$27$1187][1];
               var match$2159 = _(TS$1047[6], [ t1$1188 ]);
               var match$2160 = _(TS$1047[6], [ t2$1189 ]);
               if (!!!match$2159) { { if (!!match$2160) return 1; return __(TS$1047[7], [ t1$1188, t2$1189 ]); } }
               if (!!match$2160) return 0;
               return -1;
             });
        var swap$1190 =
          _f(function (h$1191, i$1192, i$27$1193) {
               var t$1194 = h$1191[0][i$1192];
               h$1191[0][i$1192] = h$1191[0][i$27$1193];
               return h$1191[0][i$27$1193] = t$1194;
             });
        var rem_last$1195 = _f(function (h$1196) { var l$1197 = h$1196[1] - 1; h$1196[1] = l$1197; return h$1196[0][l$1197] = 0; });
        var down$1198 =
          _f(function (h$1199, i$1200) {
               var last$1201 = _(size$1182, [ h$1199 ]) - 1;
               var start$1202 = 2 * i$1200;
               var l$1203 = start$1202 + 1;
               var r$1204 = start$1202 + 2;
               if (l$1203 > last$1201) return 0;
               var child$1205 =
                 r$1204 > last$1201 ? l$1203 : _(compare_down$1184, [ h$1199, l$1203, r$1204 ]) < 0 ? l$1203 : r$1204;
               if (_(compare_down$1184, [ h$1199, i$1200, child$1205 ]) > 0) {
                 { _(swap$1190, [ h$1199, i$1200, child$1205 ]); return __(down$1198, [ h$1199, child$1205 ]); }
               }
               return 0;
             });
        var up$1206 =
          _f(function (h$1207, i$1208) {
               var aux$1209 =
                 _f(function (h$1210, i$1211, last_spliced_out$1212) {
                      if (!(i$1211 === 0)) {
                        {
                          var p$1213 = (i$1211 - 1) / 2 >> 0;
                          var t1$1214 = h$1210[0][i$1211][1];
                          var t2$1215 = h$1210[0][p$1213][1];
                          var match$2155 = _(TS$1047[6], [ t1$1214 ]);
                          var match$2156 = _(TS$1047[6], [ t2$1215 ]);
                          if (!!match$2155) throw $(Assert_failure$26g, $("froc_ddg.ml", 209, 21));
                          if (!!match$2156) {
                            { _(swap$1190, [ h$1210, i$1211, p$1213 ]); return __(aux$1209, [ h$1210, p$1213, true ]); }
                          }
                          if (_(TS$1047[7], [ t1$1214, t2$1215 ]) < 0) {
                            { _(swap$1190, [ h$1210, i$1211, p$1213 ]); return __(aux$1209, [ h$1210, p$1213, false ]); }
                          }
                          if (last_spliced_out$1212) return __(down$1198, [ h$1210, i$1211 ]);
                          return 0;
                        }
                      }
                      if (last_spliced_out$1212) return __(down$1198, [ h$1210, 0 ]);
                      return 0;
                    });
               return __(aux$1209, [ h$1207, i$1208, false ]);
             });
        var grow$1216 = _f(function (h$1217) { var len$1218 = 2 * h$1217[1] + 1; return (h$1217[0]).length = len$1218; });
        var add$1219 =
          _f(function (h$1220, n$1221) {
               if (h$1220[1] === (h$1220[0]).length) _(grow$1216, [ h$1220 ]); else;
               h$1220[0][h$1220[1]] = n$1221;
               h$1220[1] = h$1220[1] + 1;
               return __(up$1206, [ h$1220, _(size$1182, [ h$1220 ]) - 1 ]);
             });
        var remove_min$1222 =
          _f(function (h$1223) {
               var s$1224 = _(size$1182, [ h$1223 ]);
               if (s$1224 === 0) return 0;
               if (s$1224 > 1) {
                 { h$1223[0][0] = h$1223[0][s$1224 - 1]; _(rem_last$1195, [ h$1223 ]); return __(down$1198, [ h$1223, 0 ]); }
               }
               return __(rem_last$1195, [ h$1223 ]);
             });
        var find_min$1225 =
          _f(function (h$1226) { if (_(is_empty$1180, [ h$1226 ])) throw $(Not_found$20g); return h$1226[0][0]; });
        return $(make$1179, is_empty$1180, add$1219, find_min$1225, remove_min$1222);
      }();
    var pq$1236 = $(_(PQ$1235[0], [ 0 ]));
    var init$1237 = _f(function (param$2154) { _(TS$1047[0], [ 0 ]); return pq$1236[0] = _(PQ$1235[0], [ 0 ]); });
    var enqueue$1238 = _f(function (e$1239) { return __(PQ$1235[2], [ pq$1236[0], e$1239 ]); });
    var read_now$1240 =
      _f(function ($2Aopt$2A$1241, read$1244) {
           var now$1242 = $2Aopt$2A$1241 ? $2Aopt$2A$1241[0] : true;
           if (now$1242) return read$1244;
           var notify$1245 = $(false);
           return _f(function (param$2153) { if (!notify$1245[0]) return notify$1245[0] = true; return __(read$1244, [ 0 ]); });
         });
    var add_reader_cancel$1246 =
      _f(function (now$1247, t$1248, read$1249) {
           var read$1250 = _(read_now$1240, [ now$1247, read$1249 ]);
           var start$1251 = _(TS$1047[1], [ 0 ]);
           _(read$1250, [ 0 ]);
           var r$1252 = $(read$1250, start$1251, _(TS$1047[1], [ 0 ]));
           var dep$1253 = _f(function (param$2151) { return __(enqueue$1238, [ r$1252 ]); });
           return __(add_dep_cancel$1156, [ start$1251, t$1248, dep$1253 ]);
         });
    var add_reader$1254 =
      _f(function (now$1255, t$1256, read$1257) {
           var match$2150 = _(add_reader_cancel$1246, [ now$1255, t$1256, read$1257 ]);
           return 0;
         });
    var connect_cancel$1258 =
      _f(function (u$1259, t$27$1260) {
           _(write_result$1121, [ u$1259, _(read_result$1142, [ t$27$1260 ]) ]);
           return __(add_dep_cancel$1156, [ _(TS$1047[1], [ 0 ]), t$27$1260, _(write_result_no_eq$1131, [ u$1259 ]) ]);
         });
    var connect$1261 =
      _f(function (u$1262, t$27$1263) {
           _(write_result$1121, [ u$1262, _(read_result$1142, [ t$27$1263 ]) ]);
           return __(add_dep$1163, [ _(TS$1047[1], [ 0 ]), t$27$1263, _(write_result_no_eq$1131, [ u$1262 ]) ]);
         });
    var notify_result_cancel$1264 =
      _f(function (now$1265, t$1266, f$1267) {
           return __(add_reader_cancel$1246,
                     [
                       now$1265,
                       t$1266,
                       _f(function (param$2149) {
                            try {
                              return _(f$1267, [ _(read_result$1142, [ t$1266 ]) ]);
                            }
                            catch (e$1268) {
                              return __(handle_exn$1051[0], [ e$1268 ]);
                            }
                          })
                     ]);
         });
    var notify_result$1269 =
      _f(function (now$1270, t$1271, f$1272) {
           var match$2148 = _(notify_result_cancel$1264, [ now$1270, t$1271, f$1272 ]);
           return 0;
         });
    var notify_cancel$1273 =
      _f(function (now$1274, t$1275, f$1276) {
           return __(notify_result_cancel$1264,
                     [
                       now$1274,
                       t$1275,
                       _f(function (param$2146) {
                            switch ($t(param$2146))
                            {
                            case 0: return __(f$1276, [ param$2146[0] ]);
                            case 1: return 0;
                            default: return null;
                            }
                          })
                     ]);
         });
    var notify$1278 =
      _f(function (now$1279, t$1280, f$1281) {
           return __(notify_result$1269,
                     [
                       now$1279,
                       t$1280,
                       _f(function (param$2144) {
                            switch ($t(param$2144))
                            {
                            case 0: return __(f$1281, [ param$2144[0] ]);
                            case 1: return 0;
                            default: return null;
                            }
                          })
                     ]);
         });
    var cleanup$1283 = _f(function (f$1284) { return __(TS$1047[4], [ _(TS$1047[1], [ 0 ]), f$1284 ]); });
    var bind_gen$1286 =
      _f(function (eq$1287, return$1288, assign$1289, f$1290, t$1291) {
           var match$2141 = t$1291;
           switch ($t(match$2141))
           {
           case 0:
             var match$2143 = match$2141[1];
             switch ($t(match$2143))
             {
             case 0:
               try {
                 return _(return$1288, [ _(f$1290, [ match$2143[0] ]) ]);
               }
               catch (e$1294) {
                 return __(fail$1114, [ e$1294 ]);
               }
               break;
             case 1: return __(fail$1114, [ match$2143[0] ]);
             default: return null;
             }
             break;
           default:
             var match$2140 = _(make_changeable$1099, [ eq$1287, 0, 0 ]);
             var ru$1296 = match$2140[1];
             _(add_reader$1254,
               [
                 0,
                 t$1291,
                 _f(function (param$2138) {
                      var match$2139 = _(read_result$1142, [ t$1291 ]);
                      switch ($t(match$2139))
                      {
                      case 0:
                        try {
                          return _(assign$1289, [ ru$1296, _(f$1290, [ match$2139[0] ]) ]);
                        }
                        catch (e$1299) {
                          return __(write_exn$1139, [ ru$1296, e$1299 ]);
                        }
                        break;
                      case 1: return __(write_exn$1139, [ ru$1296, match$2139[0] ]);
                      default: return null;
                      }
                    })
               ]);
             return match$2140[0];
           }
         });
    var bind$1300 =
      _f(function (eq$1301, t$1302, f$1303) {
           return __(bind_gen$1286, [ eq$1301, _f(function (prim$2137) { return prim$2137; }), connect$1261, f$1303, t$1302 ]);
         });
    var $3E$3E$3D$1304 = _f(function (t$1305, f$1306) { return __(bind$1300, [ 0, t$1305, f$1306 ]); });
    var lift$1307 = _f(function (eq$1308, f$1309) { return __(bind_gen$1286, [ eq$1308, return$1112, write$1136, f$1309 ]); });
    var blift$1310 = _f(function (eq$1311, t$1312, f$1313) { return __(lift$1307, [ eq$1311, f$1313, t$1312 ]); });
    var try_bind_gen$1314 =
      _f(function (eq$1315, return$1316, assign$1317, f$1318, succ$1319, err$1320) {
           var t$1321 = function () { try { return _(f$1318, [ 0 ]); } catch (e$1322) { return _(fail$1114, [ e$1322 ]); } }();
           var match$2134 = t$1321;
           switch ($t(match$2134))
           {
           case 0:
             var match$2136 = match$2134[1];
             switch ($t(match$2136))
             {
             case 0:
               try {
                 return _(return$1316, [ _(succ$1319, [ match$2136[0] ]) ]);
               }
               catch (e$1326) {
                 return __(fail$1114, [ e$1326 ]);
               }
               break;
             case 1:
               try {
                 return _(return$1316, [ _(err$1320, [ match$2136[0] ]) ]);
               }
               catch (e$1325) {
                 return __(fail$1114, [ e$1325 ]);
               }
               break;
             default: return null;
             }
             break;
           default:
             var match$2133 = _(make_changeable$1099, [ eq$1315, 0, 0 ]);
             var ru$1328 = match$2133[1];
             _(add_reader$1254,
               [
                 0,
                 t$1321,
                 _f(function (param$2131) {
                      try {
                        return _(assign$1317,
                                 [
                                   ru$1328,
                                   function () {
                                     var match$2132 = _(read_result$1142, [ t$1321 ]);
                                     switch ($t(match$2132))
                                     {
                                     case 0: return _(succ$1319, [ match$2132[0] ]);
                                     case 1: return _(err$1320, [ match$2132[0] ]);
                                     default: return null;
                                     }
                                   }()
                                 ]);
                      }
                      catch (e$1331) {
                        return __(write_exn$1139, [ ru$1328, e$1331 ]);
                      }
                    })
               ]);
             return match$2133[0];
           }
         });
    var try_bind$1332 =
      _f(function (eq$1333, f$1334, succ$1335, err$1336) {
           return __(try_bind_gen$1314,
                     [ eq$1333, _f(function (prim$2130) { return prim$2130; }), connect$1261, f$1334, succ$1335, err$1336 ]);
         });
    var try_bind_lift$1337 =
      _f(function (eq$1338, f$1339, succ$1340, err$1341) {
           return __(try_bind_gen$1314, [ eq$1338, return$1112, write$1136, f$1339, succ$1340, err$1341 ]);
         });
    var catch_gen$1342 =
      _f(function (eq$1343, return$1344, assign$1345, f$1346, err$1347) {
           var t$1348 = function () { try { return _(f$1346, [ 0 ]); } catch (e$1349) { return _(fail$1114, [ e$1349 ]); } }();
           var match$2126 = t$1348;
           switch ($t(match$2126))
           {
           case 0:
             var match$2128 = match$2126[1];
             switch ($t(match$2128))
             {
             case 0: return t$1348;
             case 1:
               try {
                 return _(return$1344, [ _(err$1347, [ match$2128[0] ]) ]);
               }
               catch (e$1351) {
                 return __(fail$1114, [ e$1351 ]);
               }
               break;
             default: return null;
             }
             break;
           default:
             var match$2125 = _(make_changeable$1099, [ eq$1343, 0, 0 ]);
             var ru$1353 = match$2125[1];
             _(add_reader$1254,
               [
                 0,
                 t$1348,
                 _f(function (param$2123) {
                      var r$1354 = _(read_result$1142, [ t$1348 ]);
                      switch ($t(r$1354))
                      {
                      case 0: return __(write_result$1121, [ ru$1353, r$1354 ]);
                      case 1:
                        try {
                          return _(assign$1345, [ ru$1353, _(err$1347, [ r$1354[0] ]) ]);
                        }
                        catch (e$1356) {
                          return __(write_exn$1139, [ ru$1353, e$1356 ]);
                        }
                        break;
                      default: return null;
                      }
                    })
               ]);
             return match$2125[0];
           }
         });
    var catch$1357 =
      _f(function (eq$1358, f$1359, err$1360) {
           return __(catch_gen$1342, [ eq$1358, _f(function (prim$2122) { return prim$2122; }), connect$1261, f$1359, err$1360 ]);
         });
    var catch_lift$1361 =
      _f(function (eq$1362, f$1363, err$1364) {
           return __(catch_gen$1342, [ eq$1362, return$1112, write$1136, f$1363, err$1364 ]);
         });
    var finish$1365 = _(oc$Stack$[1], [ 0 ]);
    var prop$1366 =
      _f(function (until$1367, param$2121) {
           if (!_(PQ$1235[1], [ pq$1236[0] ])) {
             {
               var r$1368 = _(PQ$1235[3], [ pq$1236[0] ]);
               if (_(TS$1047[6], [ r$1368[1] ])) { { _(PQ$1235[4], [ pq$1236[0] ]); return __(prop$1366, [ until$1367, 0 ]); } }
               var $r162 = false;
               r$162: {
                 {
                   if (!until$1367) { { $r162 = true; break r$162; } }
                   if (!(_(TS$1047[7], [ r$1368[1], until$1367[0] ]) === 1)) { { $r162 = true; break r$162; } }
                   return 0;
                 }
               }
               if ($r162) {
                 {
                   _(PQ$1235[4], [ pq$1236[0] ]);
                   _(TS$1047[3], [ r$1368[1] ]);
                   _(oc$Stack$[2], [ r$1368[2], finish$1365 ]);
                   _(r$1368[0], [ 0 ]);
                   _(oc$Stack$[3], [ finish$1365 ]);
                   _(TS$1047[5], [ _(TS$1047[2], [ 0 ]), r$1368[2] ]);
                   return __(prop$1366, [ until$1367, 0 ]);
                 }
               }
             }
           }
           return 0;
         });
    var propagate$1370 =
      _f(function (param$2120) {
           var now$27$1371 = _(TS$1047[2], [ 0 ]);
           _(prop$1366, [ 0, 0 ]);
           return __(TS$1047[3], [ now$27$1371 ]);
         });
    var memo$1382 =
      _f(function (size$1383, hash$1384, eq$1385, param$2116) {
           var h$1386 = _(oc$Froc_hashtbl$[0], [ size$1383, hash$1384, eq$1385, 0 ]);
           return _f(function (f$1387, k$1388) {
                       var result$1389 =
                         function () {
                           try {
                             if (_(oc$Stack$[7], [ finish$1365 ])) throw $(Not_found$20g); else;
                             var ok$1390 =
                               _f(function (m$1391) {
                                    return _(TS$1047[7], [ _(TS$1047[2], [ 0 ]), m$1391[1] ]) === -1 &&
                                             _(TS$1047[7], [ m$1391[2], _(oc$Stack$[4], [ finish$1365 ]) ]) === -1;
                                  });
                             var m$1392 = _(oc$Froc_hashtbl$[2], [ h$1386, k$1388, ok$1390 ]);
                             _(TS$1047[5], [ _(TS$1047[2], [ 0 ]), m$1392[1] ]);
                             _(prop$1366, [ $(m$1392[2]), 0 ]);
                             _(TS$1047[3], [ m$1392[2] ]);
                             return m$1392[0];
                           }
                           catch (exn$2117) {
                             if (exn$2117[0] === Not_found$20g) {
                               {
                                 var start$1393 = _(TS$1047[1], [ 0 ]);
                                 var result$1394 =
                                   function () { try { return $(_(f$1387, [ k$1388 ])); } catch (e$1395) { return $1(e$1395); } }();
                                 var finish$1396 = _(TS$1047[1], [ 0 ]);
                                 var m$1397 = $(result$1394, start$1393, finish$1396);
                                 _(oc$Froc_hashtbl$[1], [ h$1386, k$1388, m$1397 ]);
                                 var cancel$1398 =
                                   _f(function (param$2118) {
                                        return __(oc$Froc_hashtbl$[3],
                                                  [ h$1386, k$1388, _f(function (m$27$1399) { return m$27$1399 === m$1397; }) ]);
                                      });
                                 _(TS$1047[4], [ finish$1396, cancel$1398 ]);
                                 return result$1394;
                               }
                             }
                             throw exn$2117;
                           }
                         }();
                       switch ($t(result$1389))
                       {
                       case 0: return result$1389[0];
                       case 1: throw result$1389[0];
                       default: return null;
                       }
                     });
         });
    var add_reader2$1402 =
      _f(function (now$1403, t1$1404, t2$1405, read$1406) {
           var read$1407 = _(read_now$1240, [ now$1403, read$1406 ]);
           var start$1408 = _(TS$1047[1], [ 0 ]);
           _(read$1407, [ 0 ]);
           var r$1409 = $(read$1407, start$1408, _(TS$1047[1], [ 0 ]));
           var dep$1410 = _f(function (param$2114) { return __(enqueue$1238, [ r$1409 ]); });
           _(add_dep$1163, [ start$1408, t1$1404, dep$1410 ]);
           return __(add_dep$1163, [ start$1408, t2$1405, dep$1410 ]);
         });
    var bind2_gen$1411 =
      _f(function (eq$1412, return$1413, assign$1414, f$1415, t1$1416, t2$1417) {
           var match$2102 = t1$1416;
           var match$2103 = t2$1417;
           var $r138_0 = null;
           var $r138 = false;
           r$138: {
             {
               var $r139 = false;
               r$139: {
                 {
                   var $r140 = false;
                   r$140:
                     switch ($t(match$2102))
                     {
                     case 0:
                       var match$2109 = match$2102[1];
                       switch ($t(match$2109))
                       {
                       case 0:
                         switch ($t(match$2103))
                         {
                         case 0:
                           var match$2111 = match$2103[1];
                           switch ($t(match$2111))
                           {
                           case 0:
                             try {
                               return _(return$1413, [ _(f$1415, [ match$2109[0], match$2111[0] ]) ]);
                             }
                             catch (e$1422) {
                               return __(fail$1114, [ e$1422 ]);
                             }
                             break;
                           default: $r140 = true; break r$140;
                           }
                           break;
                         default: $r139 = true; break r$139;
                         }
                         break;
                       case 1: $r138_0 = match$2109[0]; $r138 = true; break r$138;
                       default: return null;
                       }
                       break;
                     default: $r140 = true; break r$140;
                     }
                   if ($r140)
                     switch ($t(match$2103))
                     {
                     case 0:
                       var match$2113 = match$2103[1];
                       switch ($t(match$2113))
                       {
                       case 1: $r138_0 = match$2113[0]; $r138 = true; break r$138;
                       default: $r139 = true; break r$139;
                       }
                       break;
                     default: $r139 = true; break r$139;
                     }
                 }
               }
               if ($r139) {
                 {
                   var match$2101 = _(make_changeable$1099, [ eq$1412, 0, 0 ]);
                   var ru$1424 = match$2101[1];
                   _(add_reader2$1402,
                     [
                       0,
                       t1$1416,
                       t2$1417,
                       _f(function (param$2094) {
                            var match$2095 = _(read_result$1142, [ t1$1416 ]);
                            var match$2096 = _(read_result$1142, [ t2$1417 ]);
                            var $r135_0 = null;
                            var $r135 = false;
                            r$135:
                              switch ($t(match$2095))
                              {
                              case 0:
                                switch ($t(match$2096))
                                {
                                case 0:
                                  try {
                                    return _(assign$1414, [ ru$1424, _(f$1415, [ match$2095[0], match$2096[0] ]) ]);
                                  }
                                  catch (e$1429) {
                                    return __(write_exn$1139, [ ru$1424, e$1429 ]);
                                  }
                                  break;
                                default: $r135_0 = match$2096[0]; $r135 = true; break r$135;
                                }
                                break;
                              case 1: $r135_0 = match$2095[0]; $r135 = true; break r$135;
                              default: return null;
                              }
                            if ($r135) { { var e$1425 = $r135_0; return __(write_exn$1139, [ ru$1424, e$1425 ]); } }
                          })
                     ]);
                   return match$2101[0];
                 }
               }
             }
           }
           if ($r138) { { var e$1418 = $r138_0; return __(fail$1114, [ e$1418 ]); } }
         });
    var bind2$1430 =
      _f(function (eq$1431, t1$1432, t2$1433, f$1434) {
           return __(bind2_gen$1411,
                     [ eq$1431, _f(function (prim$2093) { return prim$2093; }), connect$1261, f$1434, t1$1432, t2$1433 ]);
         });
    var lift2$1435 = _f(function (eq$1436, f$1437) { return __(bind2_gen$1411, [ eq$1436, return$1112, write$1136, f$1437 ]); });
    var blift2$1438 =
      _f(function (eq$1439, t1$1440, t2$1441, f$1442) { return __(lift2$1435, [ eq$1439, f$1442, t1$1440, t2$1441 ]); });
    var add_reader3$1443 =
      _f(function (now$1444, t1$1445, t2$1446, t3$1447, read$1448) {
           var read$1449 = _(read_now$1240, [ now$1444, read$1448 ]);
           var start$1450 = _(TS$1047[1], [ 0 ]);
           _(read$1449, [ 0 ]);
           var r$1451 = $(read$1449, start$1450, _(TS$1047[1], [ 0 ]));
           var dep$1452 = _f(function (param$2091) { return __(enqueue$1238, [ r$1451 ]); });
           _(add_dep$1163, [ start$1450, t1$1445, dep$1452 ]);
           _(add_dep$1163, [ start$1450, t2$1446, dep$1452 ]);
           return __(add_dep$1163, [ start$1450, t3$1447, dep$1452 ]);
         });
    var bind3_gen$1453 =
      _f(function (eq$1454, return$1455, assign$1456, f$1457, t1$1458, t2$1459, t3$1460) {
           var match$2072 = t1$1458;
           var match$2073 = t2$1459;
           var match$2074 = t3$1460;
           var $r120_0 = null;
           var $r120 = false;
           r$120: {
             {
               var $r121 = false;
               r$121: {
                 {
                   var $r122 = false;
                   r$122: {
                     {
                       var $r123 = false;
                       r$123:
                         switch ($t(match$2072))
                         {
                         case 0:
                           var match$2082 = match$2072[1];
                           switch ($t(match$2082))
                           {
                           case 0:
                             switch ($t(match$2073))
                             {
                             case 0:
                               var match$2084 = match$2073[1];
                               switch ($t(match$2084))
                               {
                               case 0:
                                 switch ($t(match$2074))
                                 {
                                 case 0:
                                   var match$2086 = match$2074[1];
                                   switch ($t(match$2086))
                                   {
                                   case 0:
                                     try {
                                       return _(return$1455, [ _(f$1457, [ match$2082[0], match$2084[0], match$2086[0] ]) ]);
                                     }
                                     catch (e$1467) {
                                       return __(fail$1114, [ e$1467 ]);
                                     }
                                     break;
                                   default: $r122 = true; break r$122;
                                   }
                                   break;
                                 default: $r121 = true; break r$121;
                                 }
                                 break;
                               default: $r123 = true; break r$123;
                               }
                               break;
                             default: $r122 = true; break r$122;
                             }
                             break;
                           case 1: $r120_0 = match$2082[0]; $r120 = true; break r$120;
                           default: return null;
                           }
                           break;
                         default: $r123 = true; break r$123;
                         }
                       if ($r123)
                         switch ($t(match$2073))
                         {
                         case 0:
                           var match$2088 = match$2073[1];
                           switch ($t(match$2088))
                           {
                           case 1: $r120_0 = match$2088[0]; $r120 = true; break r$120;
                           default: $r122 = true; break r$122;
                           }
                           break;
                         default: $r122 = true; break r$122;
                         }
                     }
                   }
                   if ($r122)
                     switch ($t(match$2074))
                     {
                     case 0:
                       var match$2090 = match$2074[1];
                       switch ($t(match$2090))
                       {
                       case 1: $r120_0 = match$2090[0]; $r120 = true; break r$120;
                       default: $r121 = true; break r$121;
                       }
                       break;
                     default: $r121 = true; break r$121;
                     }
                 }
               }
               if ($r121) {
                 {
                   var match$2071 = _(make_changeable$1099, [ eq$1454, 0, 0 ]);
                   var ru$1469 = match$2071[1];
                   _(add_reader3$1443,
                     [
                       0,
                       t1$1458,
                       t2$1459,
                       t3$1460,
                       _f(function (param$2061) {
                            var match$2062 = _(read_result$1142, [ t1$1458 ]);
                            var match$2063 = _(read_result$1142, [ t2$1459 ]);
                            var match$2064 = _(read_result$1142, [ t3$1460 ]);
                            var $r116_0 = null;
                            var $r116 = false;
                            r$116:
                              switch ($t(match$2062))
                              {
                              case 0:
                                switch ($t(match$2063))
                                {
                                case 0:
                                  switch ($t(match$2064))
                                  {
                                  case 0:
                                    try {
                                      return _(assign$1456,
                                               [ ru$1469, _(f$1457, [ match$2062[0], match$2063[0], match$2064[0] ]) ]);
                                    }
                                    catch (e$1476) {
                                      return __(write_exn$1139, [ ru$1469, e$1476 ]);
                                    }
                                    break;
                                  default: $r116_0 = match$2064[0]; $r116 = true; break r$116;
                                  }
                                  break;
                                default: $r116_0 = match$2063[0]; $r116 = true; break r$116;
                                }
                                break;
                              case 1: $r116_0 = match$2062[0]; $r116 = true; break r$116;
                              default: return null;
                              }
                            if ($r116) { { var e$1470 = $r116_0; return __(write_exn$1139, [ ru$1469, e$1470 ]); } }
                          })
                     ]);
                   return match$2071[0];
                 }
               }
             }
           }
           if ($r120) { { var e$1461 = $r120_0; return __(fail$1114, [ e$1461 ]); } }
         });
    var bind3$1477 =
      _f(function (eq$1478, t1$1479, t2$1480, t3$1481, f$1482) {
           return __(bind3_gen$1453,
                     [ eq$1478, _f(function (prim$2060) { return prim$2060; }), connect$1261, f$1482, t1$1479, t2$1480, t3$1481 ]);
         });
    var lift3$1483 = _f(function (eq$1484, f$1485) { return __(bind3_gen$1453, [ eq$1484, return$1112, write$1136, f$1485 ]); });
    var blift3$1486 =
      _f(function (eq$1487, t1$1488, t2$1489, t3$1490, f$1491) {
           return __(lift3$1483, [ eq$1487, f$1491, t1$1488, t2$1489, t3$1490 ]);
         });
    var add_reader4$1492 =
      _f(function (now$1493, t1$1494, t2$1495, t3$1496, t4$1497, read$1498) {
           var read$1499 = _(read_now$1240, [ now$1493, read$1498 ]);
           var start$1500 = _(TS$1047[1], [ 0 ]);
           _(read$1499, [ 0 ]);
           var r$1501 = $(read$1499, start$1500, _(TS$1047[1], [ 0 ]));
           var dep$1502 = _f(function (param$2058) { return __(enqueue$1238, [ r$1501 ]); });
           _(add_dep$1163, [ start$1500, t1$1494, dep$1502 ]);
           _(add_dep$1163, [ start$1500, t2$1495, dep$1502 ]);
           _(add_dep$1163, [ start$1500, t3$1496, dep$1502 ]);
           return __(add_dep$1163, [ start$1500, t4$1497, dep$1502 ]);
         });
    var bind4_gen$1503 =
      _f(function (eq$1504, return$1505, assign$1506, f$1507, t1$1508, t2$1509, t3$1510, t4$1511) {
           var match$2032 = t1$1508;
           var match$2033 = t2$1509;
           var match$2034 = t3$1510;
           var match$2035 = t4$1511;
           var $r100_0 = null;
           var $r100 = false;
           r$100: {
             {
               var $r101 = false;
               r$101: {
                 {
                   var $r102 = false;
                   r$102: {
                     {
                       var $r103 = false;
                       r$103: {
                         {
                           var $r104 = false;
                           r$104:
                             switch ($t(match$2032))
                             {
                             case 0:
                               var match$2045 = match$2032[1];
                               switch ($t(match$2045))
                               {
                               case 0:
                                 switch ($t(match$2033))
                                 {
                                 case 0:
                                   var match$2047 = match$2033[1];
                                   switch ($t(match$2047))
                                   {
                                   case 0:
                                     switch ($t(match$2034))
                                     {
                                     case 0:
                                       var match$2049 = match$2034[1];
                                       switch ($t(match$2049))
                                       {
                                       case 0:
                                         switch ($t(match$2035))
                                         {
                                         case 0:
                                           var match$2051 = match$2035[1];
                                           switch ($t(match$2051))
                                           {
                                           case 0:
                                             try {
                                               return _(return$1505,
                                                        [
                                                          _(f$1507, [ match$2045[0], match$2047[0], match$2049[0], match$2051[0] ])
                                                        ]);
                                             }
                                             catch (e$1520) {
                                               return __(fail$1114, [ e$1520 ]);
                                             }
                                             break;
                                           default: $r102 = true; break r$102;
                                           }
                                           break;
                                         default: $r101 = true; break r$101;
                                         }
                                         break;
                                       default: $r103 = true; break r$103;
                                       }
                                       break;
                                     default: $r102 = true; break r$102;
                                     }
                                     break;
                                   default: $r104 = true; break r$104;
                                   }
                                   break;
                                 default: $r103 = true; break r$103;
                                 }
                                 break;
                               case 1: $r100_0 = match$2045[0]; $r100 = true; break r$100;
                               default: return null;
                               }
                               break;
                             default: $r104 = true; break r$104;
                             }
                           if ($r104)
                             switch ($t(match$2033))
                             {
                             case 0:
                               var match$2053 = match$2033[1];
                               switch ($t(match$2053))
                               {
                               case 1: $r100_0 = match$2053[0]; $r100 = true; break r$100;
                               default: $r103 = true; break r$103;
                               }
                               break;
                             default: $r103 = true; break r$103;
                             }
                         }
                       }
                       if ($r103)
                         switch ($t(match$2034))
                         {
                         case 0:
                           var match$2055 = match$2034[1];
                           switch ($t(match$2055))
                           {
                           case 1: $r100_0 = match$2055[0]; $r100 = true; break r$100;
                           default: $r102 = true; break r$102;
                           }
                           break;
                         default: $r102 = true; break r$102;
                         }
                     }
                   }
                   if ($r102)
                     switch ($t(match$2035))
                     {
                     case 0:
                       var match$2057 = match$2035[1];
                       switch ($t(match$2057))
                       {
                       case 1: $r100_0 = match$2057[0]; $r100 = true; break r$100;
                       default: $r101 = true; break r$101;
                       }
                       break;
                     default: $r101 = true; break r$101;
                     }
                 }
               }
               if ($r101) {
                 {
                   var match$2031 = _(make_changeable$1099, [ eq$1504, 0, 0 ]);
                   var ru$1522 = match$2031[1];
                   _(add_reader4$1492,
                     [
                       0,
                       t1$1508,
                       t2$1509,
                       t3$1510,
                       t4$1511,
                       _f(function (param$2018) {
                            var match$2019 = _(read_result$1142, [ t1$1508 ]);
                            var match$2020 = _(read_result$1142, [ t2$1509 ]);
                            var match$2021 = _(read_result$1142, [ t3$1510 ]);
                            var match$2022 = _(read_result$1142, [ t4$1511 ]);
                            var $r95_0 = null;
                            var $r95 = false;
                            r$95:
                              switch ($t(match$2019))
                              {
                              case 0:
                                switch ($t(match$2020))
                                {
                                case 0:
                                  switch ($t(match$2021))
                                  {
                                  case 0:
                                    switch ($t(match$2022))
                                    {
                                    case 0:
                                      try {
                                        return _(assign$1506,
                                                 [
                                                   ru$1522,
                                                   _(f$1507, [ match$2019[0], match$2020[0], match$2021[0], match$2022[0] ])
                                                 ]);
                                      }
                                      catch (e$1531) {
                                        return __(write_exn$1139, [ ru$1522, e$1531 ]);
                                      }
                                      break;
                                    default: $r95_0 = match$2022[0]; $r95 = true; break r$95;
                                    }
                                    break;
                                  default: $r95_0 = match$2021[0]; $r95 = true; break r$95;
                                  }
                                  break;
                                default: $r95_0 = match$2020[0]; $r95 = true; break r$95;
                                }
                                break;
                              case 1: $r95_0 = match$2019[0]; $r95 = true; break r$95;
                              default: return null;
                              }
                            if ($r95) { { var e$1523 = $r95_0; return __(write_exn$1139, [ ru$1522, e$1523 ]); } }
                          })
                     ]);
                   return match$2031[0];
                 }
               }
             }
           }
           if ($r100) { { var e$1512 = $r100_0; return __(fail$1114, [ e$1512 ]); } }
         });
    var bind4$1532 =
      _f(function (eq$1533, t1$1534, t2$1535, t3$1536, t4$1537, f$1538) {
           return __(bind4_gen$1503,
                     [
                       eq$1533,
                       _f(function (prim$2017) { return prim$2017; }),
                       connect$1261,
                       f$1538,
                       t1$1534,
                       t2$1535,
                       t3$1536,
                       t4$1537
                     ]);
         });
    var lift4$1539 = _f(function (eq$1540, f$1541) { return __(bind4_gen$1503, [ eq$1540, return$1112, write$1136, f$1541 ]); });
    var blift4$1542 =
      _f(function (eq$1543, t1$1544, t2$1545, t3$1546, t4$1547, f$1548) {
           return __(lift4$1539, [ eq$1543, f$1548, t1$1544, t2$1545, t3$1546, t4$1547 ]);
         });
    var add_reader5$1549 =
      _f(function (now$1550, t1$1551, t2$1552, t3$1553, t4$1554, t5$1555, read$1556) {
           var read$1557 = _(read_now$1240, [ now$1550, read$1556 ]);
           var start$1558 = _(TS$1047[1], [ 0 ]);
           _(read$1557, [ 0 ]);
           var r$1559 = $(read$1557, start$1558, _(TS$1047[1], [ 0 ]));
           var dep$1560 = _f(function (param$2015) { return __(enqueue$1238, [ r$1559 ]); });
           _(add_dep$1163, [ start$1558, t1$1551, dep$1560 ]);
           _(add_dep$1163, [ start$1558, t2$1552, dep$1560 ]);
           _(add_dep$1163, [ start$1558, t3$1553, dep$1560 ]);
           _(add_dep$1163, [ start$1558, t4$1554, dep$1560 ]);
           return __(add_dep$1163, [ start$1558, t5$1555, dep$1560 ]);
         });
    var bind5_gen$1561 =
      _f(function (eq$1562, return$1563, assign$1564, f$1565, t1$1566, t2$1567, t3$1568, t4$1569, t5$1570) {
           var match$1982 = t1$1566;
           var match$1983 = t2$1567;
           var match$1984 = t3$1568;
           var match$1985 = t4$1569;
           var match$1986 = t5$1570;
           var $r78_0 = null;
           var $r78 = false;
           r$78: {
             {
               var $r79 = false;
               r$79: {
                 {
                   var $r80 = false;
                   r$80: {
                     {
                       var $r81 = false;
                       r$81: {
                         {
                           var $r82 = false;
                           r$82: {
                             {
                               var $r83 = false;
                               r$83:
                                 switch ($t(match$1982))
                                 {
                                 case 0:
                                   var match$1998 = match$1982[1];
                                   switch ($t(match$1998))
                                   {
                                   case 0:
                                     switch ($t(match$1983))
                                     {
                                     case 0:
                                       var match$2000 = match$1983[1];
                                       switch ($t(match$2000))
                                       {
                                       case 0:
                                         switch ($t(match$1984))
                                         {
                                         case 0:
                                           var match$2002 = match$1984[1];
                                           switch ($t(match$2002))
                                           {
                                           case 0:
                                             switch ($t(match$1985))
                                             {
                                             case 0:
                                               var match$2004 = match$1985[1];
                                               switch ($t(match$2004))
                                               {
                                               case 0:
                                                 switch ($t(match$1986))
                                                 {
                                                 case 0:
                                                   var match$2006 = match$1986[1];
                                                   switch ($t(match$2006))
                                                   {
                                                   case 0:
                                                     try {
                                                       return _(return$1563,
                                                                [
                                                                  _(f$1565,
                                                                    [
                                                                    match$1998[0],
                                                                    match$2000[0],
                                                                    match$2002[0],
                                                                    match$2004[0],
                                                                    match$2006[0]
                                                                    ])
                                                                ]);
                                                     }
                                                     catch (e$1581) {
                                                       return __(fail$1114, [ e$1581 ]);
                                                     }
                                                     break;
                                                   default: $r80 = true; break r$80;
                                                   }
                                                   break;
                                                 default: $r79 = true; break r$79;
                                                 }
                                                 break;
                                               default: $r81 = true; break r$81;
                                               }
                                               break;
                                             default: $r80 = true; break r$80;
                                             }
                                             break;
                                           default: $r82 = true; break r$82;
                                           }
                                           break;
                                         default: $r81 = true; break r$81;
                                         }
                                         break;
                                       default: $r83 = true; break r$83;
                                       }
                                       break;
                                     default: $r82 = true; break r$82;
                                     }
                                     break;
                                   case 1: $r78_0 = match$1998[0]; $r78 = true; break r$78;
                                   default: return null;
                                   }
                                   break;
                                 default: $r83 = true; break r$83;
                                 }
                               if ($r83)
                                 switch ($t(match$1983))
                                 {
                                 case 0:
                                   var match$2008 = match$1983[1];
                                   switch ($t(match$2008))
                                   {
                                   case 1: $r78_0 = match$2008[0]; $r78 = true; break r$78;
                                   default: $r82 = true; break r$82;
                                   }
                                   break;
                                 default: $r82 = true; break r$82;
                                 }
                             }
                           }
                           if ($r82)
                             switch ($t(match$1984))
                             {
                             case 0:
                               var match$2010 = match$1984[1];
                               switch ($t(match$2010))
                               {
                               case 1: $r78_0 = match$2010[0]; $r78 = true; break r$78;
                               default: $r81 = true; break r$81;
                               }
                               break;
                             default: $r81 = true; break r$81;
                             }
                         }
                       }
                       if ($r81)
                         switch ($t(match$1985))
                         {
                         case 0:
                           var match$2012 = match$1985[1];
                           switch ($t(match$2012))
                           {
                           case 1: $r78_0 = match$2012[0]; $r78 = true; break r$78;
                           default: $r80 = true; break r$80;
                           }
                           break;
                         default: $r80 = true; break r$80;
                         }
                     }
                   }
                   if ($r80)
                     switch ($t(match$1986))
                     {
                     case 0:
                       var match$2014 = match$1986[1];
                       switch ($t(match$2014))
                       {
                       case 1: $r78_0 = match$2014[0]; $r78 = true; break r$78;
                       default: $r79 = true; break r$79;
                       }
                       break;
                     default: $r79 = true; break r$79;
                     }
                 }
               }
               if ($r79) {
                 {
                   var match$1981 = _(make_changeable$1099, [ eq$1562, 0, 0 ]);
                   var ru$1583 = match$1981[1];
                   _(add_reader5$1549,
                     [
                       0,
                       t1$1566,
                       t2$1567,
                       t3$1568,
                       t4$1569,
                       t5$1570,
                       _f(function (param$1965) {
                            var match$1966 = _(read_result$1142, [ t1$1566 ]);
                            var match$1967 = _(read_result$1142, [ t2$1567 ]);
                            var match$1968 = _(read_result$1142, [ t3$1568 ]);
                            var match$1969 = _(read_result$1142, [ t4$1569 ]);
                            var match$1970 = _(read_result$1142, [ t5$1570 ]);
                            var $r72_0 = null;
                            var $r72 = false;
                            r$72:
                              switch ($t(match$1966))
                              {
                              case 0:
                                switch ($t(match$1967))
                                {
                                case 0:
                                  switch ($t(match$1968))
                                  {
                                  case 0:
                                    switch ($t(match$1969))
                                    {
                                    case 0:
                                      switch ($t(match$1970))
                                      {
                                      case 0:
                                        try {
                                          return _(assign$1564,
                                                   [
                                                     ru$1583,
                                                     _(f$1565,
                                                       [
                                                         match$1966[0],
                                                         match$1967[0],
                                                         match$1968[0],
                                                         match$1969[0],
                                                         match$1970[0]
                                                       ])
                                                   ]);
                                        }
                                        catch (e$1594) {
                                          return __(write_exn$1139, [ ru$1583, e$1594 ]);
                                        }
                                        break;
                                      default: $r72_0 = match$1970[0]; $r72 = true; break r$72;
                                      }
                                      break;
                                    default: $r72_0 = match$1969[0]; $r72 = true; break r$72;
                                    }
                                    break;
                                  default: $r72_0 = match$1968[0]; $r72 = true; break r$72;
                                  }
                                  break;
                                default: $r72_0 = match$1967[0]; $r72 = true; break r$72;
                                }
                                break;
                              case 1: $r72_0 = match$1966[0]; $r72 = true; break r$72;
                              default: return null;
                              }
                            if ($r72) { { var e$1584 = $r72_0; return __(write_exn$1139, [ ru$1583, e$1584 ]); } }
                          })
                     ]);
                   return match$1981[0];
                 }
               }
             }
           }
           if ($r78) { { var e$1571 = $r78_0; return __(fail$1114, [ e$1571 ]); } }
         });
    var bind5$1595 =
      _f(function (eq$1596, t1$1597, t2$1598, t3$1599, t4$1600, t5$1601, f$1602) {
           return __(bind5_gen$1561,
                     [
                       eq$1596,
                       _f(function (prim$1964) { return prim$1964; }),
                       connect$1261,
                       f$1602,
                       t1$1597,
                       t2$1598,
                       t3$1599,
                       t4$1600,
                       t5$1601
                     ]);
         });
    var lift5$1603 = _f(function (eq$1604, f$1605) { return __(bind5_gen$1561, [ eq$1604, return$1112, write$1136, f$1605 ]); });
    var blift5$1606 =
      _f(function (eq$1607, t1$1608, t2$1609, t3$1610, t4$1611, t5$1612, f$1613) {
           return __(lift5$1603, [ eq$1607, f$1613, t1$1608, t2$1609, t3$1610, t4$1611, t5$1612 ]);
         });
    var add_reader6$1614 =
      _f(function (now$1615, t1$1616, t2$1617, t3$1618, t4$1619, t5$1620, t6$1621, read$1622) {
           var read$1623 = _(read_now$1240, [ now$1615, read$1622 ]);
           var start$1624 = _(TS$1047[1], [ 0 ]);
           _(read$1623, [ 0 ]);
           var r$1625 = $(read$1623, start$1624, _(TS$1047[1], [ 0 ]));
           var dep$1626 = _f(function (param$1962) { return __(enqueue$1238, [ r$1625 ]); });
           _(add_dep$1163, [ start$1624, t1$1616, dep$1626 ]);
           _(add_dep$1163, [ start$1624, t2$1617, dep$1626 ]);
           _(add_dep$1163, [ start$1624, t3$1618, dep$1626 ]);
           _(add_dep$1163, [ start$1624, t4$1619, dep$1626 ]);
           _(add_dep$1163, [ start$1624, t5$1620, dep$1626 ]);
           return __(add_dep$1163, [ start$1624, t6$1621, dep$1626 ]);
         });
    var bind6_gen$1627 =
      _f(function (eq$1628, return$1629, assign$1630, f$1631, t1$1632, t2$1633, t3$1634, t4$1635, t5$1636, t6$1637) {
           var match$1922 = t1$1632;
           var match$1923 = t2$1633;
           var match$1924 = t3$1634;
           var match$1925 = t4$1635;
           var match$1926 = t5$1636;
           var match$1927 = t6$1637;
           var $r54_0 = null;
           var $r54 = false;
           r$54: {
             {
               var $r55 = false;
               r$55: {
                 {
                   var $r56 = false;
                   r$56: {
                     {
                       var $r57 = false;
                       r$57: {
                         {
                           var $r58 = false;
                           r$58: {
                             {
                               var $r59 = false;
                               r$59: {
                                 {
                                   var $r60 = false;
                                   r$60:
                                     switch ($t(match$1922))
                                     {
                                     case 0:
                                       var match$1941 = match$1922[1];
                                       switch ($t(match$1941))
                                       {
                                       case 0:
                                         switch ($t(match$1923))
                                         {
                                         case 0:
                                           var match$1943 = match$1923[1];
                                           switch ($t(match$1943))
                                           {
                                           case 0:
                                             switch ($t(match$1924))
                                             {
                                             case 0:
                                               var match$1945 = match$1924[1];
                                               switch ($t(match$1945))
                                               {
                                               case 0:
                                                 switch ($t(match$1925))
                                                 {
                                                 case 0:
                                                   var match$1947 = match$1925[1];
                                                   switch ($t(match$1947))
                                                   {
                                                   case 0:
                                                     switch ($t(match$1926))
                                                     {
                                                     case 0:
                                                       var match$1949 = match$1926[1];
                                                       switch ($t(match$1949))
                                                       {
                                                       case 0:
                                                         switch ($t(match$1927))
                                                         {
                                                         case 0:
                                                           var match$1951 = match$1927[1];
                                                           switch ($t(match$1951))
                                                           {
                                                           case 0:
                                                             try {
                                                               return _
                                                                    (return$1629,
                                                                    [
                                                                    _
                                                                    (f$1631,
                                                                    [
                                                                    match$1941[0],
                                                                    match$1943[0],
                                                                    match$1945[0],
                                                                    match$1947[0],
                                                                    match$1949[0],
                                                                    match$1951[0]
                                                                    ])
                                                                    ]);
                                                             }
                                                             catch (e$1650) {
                                                               return __(fail$1114, [ e$1650 ]);
                                                             }
                                                             break;
                                                           default: $r56 = true; break r$56;
                                                           }
                                                           break;
                                                         default: $r55 = true; break r$55;
                                                         }
                                                         break;
                                                       default: $r57 = true; break r$57;
                                                       }
                                                       break;
                                                     default: $r56 = true; break r$56;
                                                     }
                                                     break;
                                                   default: $r58 = true; break r$58;
                                                   }
                                                   break;
                                                 default: $r57 = true; break r$57;
                                                 }
                                                 break;
                                               default: $r59 = true; break r$59;
                                               }
                                               break;
                                             default: $r58 = true; break r$58;
                                             }
                                             break;
                                           default: $r60 = true; break r$60;
                                           }
                                           break;
                                         default: $r59 = true; break r$59;
                                         }
                                         break;
                                       case 1: $r54_0 = match$1941[0]; $r54 = true; break r$54;
                                       default: return null;
                                       }
                                       break;
                                     default: $r60 = true; break r$60;
                                     }
                                   if ($r60)
                                     switch ($t(match$1923))
                                     {
                                     case 0:
                                       var match$1953 = match$1923[1];
                                       switch ($t(match$1953))
                                       {
                                       case 1: $r54_0 = match$1953[0]; $r54 = true; break r$54;
                                       default: $r59 = true; break r$59;
                                       }
                                       break;
                                     default: $r59 = true; break r$59;
                                     }
                                 }
                               }
                               if ($r59)
                                 switch ($t(match$1924))
                                 {
                                 case 0:
                                   var match$1955 = match$1924[1];
                                   switch ($t(match$1955))
                                   {
                                   case 1: $r54_0 = match$1955[0]; $r54 = true; break r$54;
                                   default: $r58 = true; break r$58;
                                   }
                                   break;
                                 default: $r58 = true; break r$58;
                                 }
                             }
                           }
                           if ($r58)
                             switch ($t(match$1925))
                             {
                             case 0:
                               var match$1957 = match$1925[1];
                               switch ($t(match$1957))
                               {
                               case 1: $r54_0 = match$1957[0]; $r54 = true; break r$54;
                               default: $r57 = true; break r$57;
                               }
                               break;
                             default: $r57 = true; break r$57;
                             }
                         }
                       }
                       if ($r57)
                         switch ($t(match$1926))
                         {
                         case 0:
                           var match$1959 = match$1926[1];
                           switch ($t(match$1959))
                           {
                           case 1: $r54_0 = match$1959[0]; $r54 = true; break r$54;
                           default: $r56 = true; break r$56;
                           }
                           break;
                         default: $r56 = true; break r$56;
                         }
                     }
                   }
                   if ($r56)
                     switch ($t(match$1927))
                     {
                     case 0:
                       var match$1961 = match$1927[1];
                       switch ($t(match$1961))
                       {
                       case 1: $r54_0 = match$1961[0]; $r54 = true; break r$54;
                       default: $r55 = true; break r$55;
                       }
                       break;
                     default: $r55 = true; break r$55;
                     }
                 }
               }
               if ($r55) {
                 {
                   var match$1921 = _(make_changeable$1099, [ eq$1628, 0, 0 ]);
                   var ru$1652 = match$1921[1];
                   _(add_reader6$1614,
                     [
                       0,
                       t1$1632,
                       t2$1633,
                       t3$1634,
                       t4$1635,
                       t5$1636,
                       t6$1637,
                       _f(function (param$1902) {
                            var match$1903 = _(read_result$1142, [ t1$1632 ]);
                            var match$1904 = _(read_result$1142, [ t2$1633 ]);
                            var match$1905 = _(read_result$1142, [ t3$1634 ]);
                            var match$1906 = _(read_result$1142, [ t4$1635 ]);
                            var match$1907 = _(read_result$1142, [ t5$1636 ]);
                            var match$1908 = _(read_result$1142, [ t6$1637 ]);
                            var $r47_0 = null;
                            var $r47 = false;
                            r$47:
                              switch ($t(match$1903))
                              {
                              case 0:
                                switch ($t(match$1904))
                                {
                                case 0:
                                  switch ($t(match$1905))
                                  {
                                  case 0:
                                    switch ($t(match$1906))
                                    {
                                    case 0:
                                      switch ($t(match$1907))
                                      {
                                      case 0:
                                        switch ($t(match$1908))
                                        {
                                        case 0:
                                          try {
                                            return _(assign$1630,
                                                     [
                                                       ru$1652,
                                                       _(f$1631,
                                                         [
                                                           match$1903[0],
                                                           match$1904[0],
                                                           match$1905[0],
                                                           match$1906[0],
                                                           match$1907[0],
                                                           match$1908[0]
                                                         ])
                                                     ]);
                                          }
                                          catch (e$1665) {
                                            return __(write_exn$1139, [ ru$1652, e$1665 ]);
                                          }
                                          break;
                                        default: $r47_0 = match$1908[0]; $r47 = true; break r$47;
                                        }
                                        break;
                                      default: $r47_0 = match$1907[0]; $r47 = true; break r$47;
                                      }
                                      break;
                                    default: $r47_0 = match$1906[0]; $r47 = true; break r$47;
                                    }
                                    break;
                                  default: $r47_0 = match$1905[0]; $r47 = true; break r$47;
                                  }
                                  break;
                                default: $r47_0 = match$1904[0]; $r47 = true; break r$47;
                                }
                                break;
                              case 1: $r47_0 = match$1903[0]; $r47 = true; break r$47;
                              default: return null;
                              }
                            if ($r47) { { var e$1653 = $r47_0; return __(write_exn$1139, [ ru$1652, e$1653 ]); } }
                          })
                     ]);
                   return match$1921[0];
                 }
               }
             }
           }
           if ($r54) { { var e$1638 = $r54_0; return __(fail$1114, [ e$1638 ]); } }
         });
    var bind6$1666 =
      _f(function (eq$1667, t1$1668, t2$1669, t3$1670, t4$1671, t5$1672, t6$1673, f$1674) {
           return __(bind6_gen$1627,
                     [
                       eq$1667,
                       _f(function (prim$1901) { return prim$1901; }),
                       connect$1261,
                       f$1674,
                       t1$1668,
                       t2$1669,
                       t3$1670,
                       t4$1671,
                       t5$1672,
                       t6$1673
                     ]);
         });
    var lift6$1675 = _f(function (eq$1676, f$1677) { return __(bind6_gen$1627, [ eq$1676, return$1112, write$1136, f$1677 ]); });
    var blift6$1678 =
      _f(function (eq$1679, t1$1680, t2$1681, t3$1682, t4$1683, t5$1684, t6$1685, f$1686) {
           return __(lift6$1675, [ eq$1679, f$1686, t1$1680, t2$1681, t3$1682, t4$1683, t5$1684, t6$1685 ]);
         });
    var add_reader7$1687 =
      _f(function (now$1688, t1$1689, t2$1690, t3$1691, t4$1692, t5$1693, t6$1694, t7$1695, read$1696) {
           var read$1697 = _(read_now$1240, [ now$1688, read$1696 ]);
           var start$1698 = _(TS$1047[1], [ 0 ]);
           _(read$1697, [ 0 ]);
           var r$1699 = $(read$1697, start$1698, _(TS$1047[1], [ 0 ]));
           var dep$1700 = _f(function (param$1899) { return __(enqueue$1238, [ r$1699 ]); });
           _(add_dep$1163, [ start$1698, t1$1689, dep$1700 ]);
           _(add_dep$1163, [ start$1698, t2$1690, dep$1700 ]);
           _(add_dep$1163, [ start$1698, t3$1691, dep$1700 ]);
           _(add_dep$1163, [ start$1698, t4$1692, dep$1700 ]);
           _(add_dep$1163, [ start$1698, t5$1693, dep$1700 ]);
           _(add_dep$1163, [ start$1698, t6$1694, dep$1700 ]);
           return __(add_dep$1163, [ start$1698, t7$1695, dep$1700 ]);
         });
    var bind7_gen$1701 =
      _f(function (eq$1702, return$1703, assign$1704, f$1705, t1$1706, t2$1707, t3$1708, t4$1709, t5$1710, t6$1711, t7$1712) {
           var match$1852 = t1$1706;
           var match$1853 = t2$1707;
           var match$1854 = t3$1708;
           var match$1855 = t4$1709;
           var match$1856 = t5$1710;
           var match$1857 = t6$1711;
           var match$1858 = t7$1712;
           var $r28_0 = null;
           var $r28 = false;
           r$28: {
             {
               var $r29 = false;
               r$29: {
                 {
                   var $r30 = false;
                   r$30: {
                     {
                       var $r31 = false;
                       r$31: {
                         {
                           var $r32 = false;
                           r$32: {
                             {
                               var $r33 = false;
                               r$33: {
                                 {
                                   var $r34 = false;
                                   r$34: {
                                     {
                                       var $r35 = false;
                                       r$35:
                                         switch ($t(match$1852))
                                         {
                                         case 0:
                                           var match$1874 = match$1852[1];
                                           switch ($t(match$1874))
                                           {
                                           case 0:
                                             switch ($t(match$1853))
                                             {
                                             case 0:
                                               var match$1876 = match$1853[1];
                                               switch ($t(match$1876))
                                               {
                                               case 0:
                                                 switch ($t(match$1854))
                                                 {
                                                 case 0:
                                                   var match$1878 = match$1854[1];
                                                   switch ($t(match$1878))
                                                   {
                                                   case 0:
                                                     switch ($t(match$1855))
                                                     {
                                                     case 0:
                                                       var match$1880 = match$1855[1];
                                                       switch ($t(match$1880))
                                                       {
                                                       case 0:
                                                         switch ($t(match$1856))
                                                         {
                                                         case 0:
                                                           var match$1882 = match$1856[1];
                                                           switch ($t(match$1882))
                                                           {
                                                           case 0:
                                                             switch (
                                                             $t(match$1857))
                                                             {
                                                             case 0:
                                                               var match$1884 = match$1857[1];
                                                               switch (
                                                               $t(match$1884))
                                                               {
                                                               case 0:
                                                                 switch (
                                                                 $t(match$1858))
                                                                 {
                                                                 case 0:
                                                                   var match$1886 = match$1858[1];
                                                                   switch (
                                                                   $t(match$1886))
                                                                   {
                                                                   case 0:
                                                                    try {
                                                                    return _
                                                                    (return$1703,
                                                                    [
                                                                    _
                                                                    (f$1705,
                                                                    [
                                                                    match$1874[0],
                                                                    match$1876[0],
                                                                    match$1878[0],
                                                                    match$1880[0],
                                                                    match$1882[0],
                                                                    match$1884[0],
                                                                    match$1886[0]
                                                                    ])
                                                                    ]);
                                                                    }
                                                                    catch (e$1727) {
                                                                    return __(fail$1114, [ e$1727 ]);
                                                                    }
                                                                    break;
                                                                   default: $r30 = true; break r$30;
                                                                   }
                                                                   break;
                                                                 default: $r29 = true; break r$29;
                                                                 }
                                                                 break;
                                                               default: $r31 = true; break r$31;
                                                               }
                                                               break;
                                                             default: $r30 = true; break r$30;
                                                             }
                                                             break;
                                                           default: $r32 = true; break r$32;
                                                           }
                                                           break;
                                                         default: $r31 = true; break r$31;
                                                         }
                                                         break;
                                                       default: $r33 = true; break r$33;
                                                       }
                                                       break;
                                                     default: $r32 = true; break r$32;
                                                     }
                                                     break;
                                                   default: $r34 = true; break r$34;
                                                   }
                                                   break;
                                                 default: $r33 = true; break r$33;
                                                 }
                                                 break;
                                               default: $r35 = true; break r$35;
                                               }
                                               break;
                                             default: $r34 = true; break r$34;
                                             }
                                             break;
                                           case 1: $r28_0 = match$1874[0]; $r28 = true; break r$28;
                                           default: return null;
                                           }
                                           break;
                                         default: $r35 = true; break r$35;
                                         }
                                       if ($r35)
                                         switch ($t(match$1853))
                                         {
                                         case 0:
                                           var match$1888 = match$1853[1];
                                           switch ($t(match$1888))
                                           {
                                           case 1: $r28_0 = match$1888[0]; $r28 = true; break r$28;
                                           default: $r34 = true; break r$34;
                                           }
                                           break;
                                         default: $r34 = true; break r$34;
                                         }
                                     }
                                   }
                                   if ($r34)
                                     switch ($t(match$1854))
                                     {
                                     case 0:
                                       var match$1890 = match$1854[1];
                                       switch ($t(match$1890))
                                       {
                                       case 1: $r28_0 = match$1890[0]; $r28 = true; break r$28;
                                       default: $r33 = true; break r$33;
                                       }
                                       break;
                                     default: $r33 = true; break r$33;
                                     }
                                 }
                               }
                               if ($r33)
                                 switch ($t(match$1855))
                                 {
                                 case 0:
                                   var match$1892 = match$1855[1];
                                   switch ($t(match$1892))
                                   {
                                   case 1: $r28_0 = match$1892[0]; $r28 = true; break r$28;
                                   default: $r32 = true; break r$32;
                                   }
                                   break;
                                 default: $r32 = true; break r$32;
                                 }
                             }
                           }
                           if ($r32)
                             switch ($t(match$1856))
                             {
                             case 0:
                               var match$1894 = match$1856[1];
                               switch ($t(match$1894))
                               {
                               case 1: $r28_0 = match$1894[0]; $r28 = true; break r$28;
                               default: $r31 = true; break r$31;
                               }
                               break;
                             default: $r31 = true; break r$31;
                             }
                         }
                       }
                       if ($r31)
                         switch ($t(match$1857))
                         {
                         case 0:
                           var match$1896 = match$1857[1];
                           switch ($t(match$1896))
                           {
                           case 1: $r28_0 = match$1896[0]; $r28 = true; break r$28;
                           default: $r30 = true; break r$30;
                           }
                           break;
                         default: $r30 = true; break r$30;
                         }
                     }
                   }
                   if ($r30)
                     switch ($t(match$1858))
                     {
                     case 0:
                       var match$1898 = match$1858[1];
                       switch ($t(match$1898))
                       {
                       case 1: $r28_0 = match$1898[0]; $r28 = true; break r$28;
                       default: $r29 = true; break r$29;
                       }
                       break;
                     default: $r29 = true; break r$29;
                     }
                 }
               }
               if ($r29) {
                 {
                   var match$1851 = _(make_changeable$1099, [ eq$1702, 0, 0 ]);
                   var ru$1729 = match$1851[1];
                   _(add_reader7$1687,
                     [
                       0,
                       t1$1706,
                       t2$1707,
                       t3$1708,
                       t4$1709,
                       t5$1710,
                       t6$1711,
                       t7$1712,
                       _f(function (param$1829) {
                            var match$1830 = _(read_result$1142, [ t1$1706 ]);
                            var match$1831 = _(read_result$1142, [ t2$1707 ]);
                            var match$1832 = _(read_result$1142, [ t3$1708 ]);
                            var match$1833 = _(read_result$1142, [ t4$1709 ]);
                            var match$1834 = _(read_result$1142, [ t5$1710 ]);
                            var match$1835 = _(read_result$1142, [ t6$1711 ]);
                            var match$1836 = _(read_result$1142, [ t7$1712 ]);
                            var $r20_0 = null;
                            var $r20 = false;
                            r$20:
                              switch ($t(match$1830))
                              {
                              case 0:
                                switch ($t(match$1831))
                                {
                                case 0:
                                  switch ($t(match$1832))
                                  {
                                  case 0:
                                    switch ($t(match$1833))
                                    {
                                    case 0:
                                      switch ($t(match$1834))
                                      {
                                      case 0:
                                        switch ($t(match$1835))
                                        {
                                        case 0:
                                          switch ($t(match$1836))
                                          {
                                          case 0:
                                            try {
                                              return _(assign$1704,
                                                       [
                                                         ru$1729,
                                                         _(f$1705,
                                                           [
                                                             match$1830[0],
                                                             match$1831[0],
                                                             match$1832[0],
                                                             match$1833[0],
                                                             match$1834[0],
                                                             match$1835[0],
                                                             match$1836[0]
                                                           ])
                                                       ]);
                                            }
                                            catch (e$1744) {
                                              return __(write_exn$1139, [ ru$1729, e$1744 ]);
                                            }
                                            break;
                                          default: $r20_0 = match$1836[0]; $r20 = true; break r$20;
                                          }
                                          break;
                                        default: $r20_0 = match$1835[0]; $r20 = true; break r$20;
                                        }
                                        break;
                                      default: $r20_0 = match$1834[0]; $r20 = true; break r$20;
                                      }
                                      break;
                                    default: $r20_0 = match$1833[0]; $r20 = true; break r$20;
                                    }
                                    break;
                                  default: $r20_0 = match$1832[0]; $r20 = true; break r$20;
                                  }
                                  break;
                                default: $r20_0 = match$1831[0]; $r20 = true; break r$20;
                                }
                                break;
                              case 1: $r20_0 = match$1830[0]; $r20 = true; break r$20;
                              default: return null;
                              }
                            if ($r20) { { var e$1730 = $r20_0; return __(write_exn$1139, [ ru$1729, e$1730 ]); } }
                          })
                     ]);
                   return match$1851[0];
                 }
               }
             }
           }
           if ($r28) { { var e$1713 = $r28_0; return __(fail$1114, [ e$1713 ]); } }
         });
    var bind7$1745 =
      _f(function (eq$1746, t1$1747, t2$1748, t3$1749, t4$1750, t5$1751, t6$1752, t7$1753, f$1754) {
           return __(bind7_gen$1701,
                     [
                       eq$1746,
                       _f(function (prim$1828) { return prim$1828; }),
                       connect$1261,
                       f$1754,
                       t1$1747,
                       t2$1748,
                       t3$1749,
                       t4$1750,
                       t5$1751,
                       t6$1752,
                       t7$1753
                     ]);
         });
    var lift7$1755 = _f(function (eq$1756, f$1757) { return __(bind7_gen$1701, [ eq$1756, return$1112, write$1136, f$1757 ]); });
    var blift7$1758 =
      _f(function (eq$1759, t1$1760, t2$1761, t3$1762, t4$1763, t5$1764, t6$1765, t7$1766, f$1767) {
           return __(lift7$1755, [ eq$1759, f$1767, t1$1760, t2$1761, t3$1762, t4$1763, t5$1764, t6$1765, t7$1766 ]);
         });
    var add_readerN$1768 =
      _f(function (now$1769, ts$1770, read$1771) {
           var read$1772 = _(read_now$1240, [ now$1769, read$1771 ]);
           var start$1773 = _(TS$1047[1], [ 0 ]);
           _(read$1772, [ 0 ]);
           var r$1774 = $(read$1772, start$1773, _(TS$1047[1], [ 0 ]));
           var dep$1775 = _f(function (param$1826) { return __(enqueue$1238, [ r$1774 ]); });
           return __(oc$List$[9],
                     [ _f(function (t$1776) { return __(add_dep$1163, [ start$1773, t$1776, dep$1775 ]); }), ts$1770 ]);
         });
    var bindN_gen$1777 =
      _f(function (eq$1778, return$1779, assign$1780, f$1781, ts$1782) {
           var loop$1783 =
             _f(function (vs$1784, param$1820) {
                  if (param$1820) {
                    {
                      var match$1823 = param$1820[0];
                      switch ($t(match$1823))
                      {
                      case 0:
                        var match$1825 = match$1823[1];
                        switch ($t(match$1825))
                        {
                        case 0: return __(loop$1783, [ $(match$1825[0], vs$1784), param$1820[1] ]);
                        case 1: return __(fail$1114, [ match$1825[0] ]);
                        default: return null;
                        }
                        break;
                      default:
                        var match$1822 = _(make_changeable$1099, [ eq$1778, 0, 0 ]);
                        var ru$1791 = match$1822[1];
                        _(add_readerN$1768,
                          [
                            0,
                            ts$1782,
                            _f(function (param$1821) {
                                 try {
                                   var vs$1792 = _(oc$List$[10], [ read$1146, ts$1782 ]);
                                   return _(assign$1780, [ ru$1791, _(f$1781, [ vs$1792 ]) ]);
                                 }
                                 catch (e$1793) {
                                   return __(write_exn$1139, [ ru$1791, e$1793 ]);
                                 }
                               })
                          ]);
                        return match$1822[0];
                      }
                    }
                  }
                  try {
                    return _(return$1779, [ _(f$1781, [ _(oc$List$[4], [ vs$1784 ]) ]) ]);
                  }
                  catch (e$1787) {
                    return __(fail$1114, [ e$1787 ]);
                  }
                });
           return __(loop$1783, [ 0, ts$1782 ]);
         });
    var bindN$1794 =
      _f(function (eq$1795, ts$1796, f$1797) {
           return __(bindN_gen$1777, [ eq$1795, _f(function (prim$1819) { return prim$1819; }), connect$1261, f$1797, ts$1796 ]);
         });
    var liftN$1798 = _f(function (eq$1799, f$1800) { return __(bindN_gen$1777, [ eq$1799, return$1112, write$1136, f$1800 ]); });
    var bliftN$1801 = _f(function (eq$1802, ts$1803, f$1804) { return __(liftN$1798, [ eq$1802, f$1804, ts$1803 ]); });
    return $(Unset$1094, make_cancel$1151, no_cancel$1153, cancel$1154, changeable$1109, return$1112, fail$1114, is_constant$1116,
             bind$1300, $3E$3E$3D$1304, lift$1307, blift$1310, add_reader$1254, add_reader_cancel$1246, catch$1357, try_bind$1332,
             catch_lift$1361, try_bind_lift$1337, read$1146, read_result$1142, write$1136, write_exn$1139, write_result$1121,
             clear$1118, notify$1278, notify_cancel$1273, notify_result$1269, notify_result_cancel$1264, connect$1261,
             connect_cancel$1258, cleanup$1283, make_changeable$1099, make_constant$1107, hash$1090, init$1237, propagate$1370,
             set_exn_handler$1052, set_debug$1049, memo$1382, bind2$1430, lift2$1435, blift2$1438, add_reader2$1402, bind3$1477,
             lift3$1483, blift3$1486, add_reader3$1443, bind4$1532, lift4$1539, blift4$1542, add_reader4$1492, bind5$1595,
             lift5$1603, blift5$1606, add_reader5$1549, bind6$1666, lift6$1675, blift6$1678, add_reader6$1614, bind7$1745,
             lift7$1755, blift7$1758, add_reader7$1687, bindN$1794, liftN$1798, bliftN$1801, add_readerN$1768);
  }();
var oc$Froc$ =
  function () {
    var include$1290 = oc$Froc_ddg$;
    var Unset$1032 = include$1290[0];
    var no_cancel$1035 = include$1290[2];
    var cancel$1036 = include$1290[3];
    var is_constant$1041 = include$1290[7];
    var bind$1042 = include$1290[8];
    var read_result$1053 = include$1290[19];
    var write_result$1056 = include$1290[22];
    var notify$1058 = include$1290[24];
    var notify_cancel$1059 = include$1290[25];
    var notify_result$1060 = include$1290[26];
    var notify_result_cancel$1061 = include$1290[27];
    var connect$1062 = include$1290[28];
    var make_changeable$1065 = include$1290[31];
    var make_constant$1066 = include$1290[32];
    var hash$1067 = include$1290[33];
    var debug$1103 = $(_f(function (prim$1334) { return 0; }));
    var set_debug$1104 = _f(function (f$1105) { debug$1103[0] = f$1105; return __(include$1290[37], [ f$1105 ]); });
    var q$1108 = _(oc$Queue$[1], [ 0 ]);
    var temps$1109 = $(0);
    var running$1110 = $(false);
    var init$1111 =
      _f(function (param$1333) {
           _(include$1290[34], [ 0 ]);
           _(oc$Queue$[8], [ q$1108 ]);
           temps$1109[0] = 0;
           return running$1110[0] = false;
         });
    var run_queue$1112 =
      _f(function (param$1332) {
           if (!running$1110[0]) {
             {
               running$1110[0] = true;
               try {
                 while (!_(oc$Queue$[10], [ q$1108 ])) _(oc$Queue$[4], [ q$1108, 0 ]);
                 return running$1110[0] = false;
               }
               catch (e$1113) {
                 running$1110[0] = false;
                 throw e$1113;
               }
             }
           }
           return 0;
         });
    var with_run_queue$1114 =
      _f(function (f$1115) {
           var running$27$1116 = running$1110[0];
           running$1110[0] = true;
           _(f$1115, [ 0 ]);
           running$1110[0] = running$27$1116;
           return __(run_queue$1112, [ 0 ]);
         });
    var write_temp_result$1117 =
      _f(function (u$1118, r$1119) {
           temps$1109[0] = $(_f(function (param$1331) { return __(include$1290[23], [ u$1118 ]); }), temps$1109[0]);
           return __(write_result$1056, [ u$1118, r$1119 ]);
         });
    var send_result$1120 =
      _f(function (s$1122, r$1123) {
           var match$1329 = temps$1109[0];
           if (match$1329) return __(send_result_deferred$1121, [ s$1122, r$1123 ]);
           return __(with_run_queue$1114,
                     [
                       _f(function (param$1328) {
                            _(write_temp_result$1117, [ s$1122, r$1123 ]);
                            _(include$1290[35], [ 0 ]);
                            _(oc$List$[9], [ _f(function (f$1124) { return __(f$1124, [ 0 ]); }), temps$1109[0] ]);
                            return temps$1109[0] = 0;
                          })
                     ]);
         });
    var send_result_deferred$1121 =
      _f(function (s$1125, r$1126) {
           _(oc$Queue$[2], [ _f(function (param$1330) { return __(send_result$1120, [ s$1125, r$1126 ]); }), q$1108 ]);
           return __(run_queue$1112, [ 0 ]);
         });
    var send$1127 = _f(function (s$1128, v$1129) { return __(send_result$1120, [ s$1128, $(v$1129) ]); });
    var send_exn$1130 = _f(function (s$1131, e$1132) { return __(send_result$1120, [ s$1131, $1(e$1132) ]); });
    var send_deferred$1133 = _f(function (s$1134, v$1135) { return __(send_result_deferred$1121, [ s$1134, $(v$1135) ]); });
    var send_exn_deferred$1136 = _f(function (s$1137, e$1138) { return __(send_result_deferred$1121, [ s$1137, $1(e$1138) ]); });
    var never_eq$1139 = _f(function (param$1326, param$1327) { return false; });
    var make_event$1140 = _f(function (param$1325) { return __(make_changeable$1065, [ $(never_eq$1139), 0, 0 ]); });
    var never$1141 = _(make_constant$1066, [ $1($(Unset$1032)) ]);
    var notify_result_e_cancel$1143 =
      _f(function (t$1144, f$1145) { return __(notify_result_cancel$1061, [ $(false), t$1144, f$1145 ]); });
    var notify_result_e$1146 = _f(function (t$1147, f$1148) { return __(notify_result$1060, [ $(false), t$1147, f$1148 ]); });
    var notify_e_cancel$1149 = _f(function (t$1150, f$1151) { return __(notify_cancel$1059, [ $(false), t$1150, f$1151 ]); });
    var notify_e$1152 = _f(function (t$1153, f$1154) { return __(notify$1058, [ $(false), t$1153, f$1154 ]); });
    var next$1156 =
      _f(function (t$1157) {
           if (_(is_constant$1041, [ t$1157 ])) return never$1141;
           var match$1324 = _(make_event$1140, [ 0 ]);
           var c$1160 = $(no_cancel$1035);
           c$1160[0] =
             _(notify_result_e_cancel$1143,
               [
                 t$1157,
                 _f(function (r$1161) {
                      _(cancel$1036, [ c$1160[0] ]);
                      c$1160[0] = no_cancel$1035;
                      return __(write_temp_result$1117, [ match$1324[1], r$1161 ]);
                    })
               ]);
           return match$1324[0];
         });
    var merge$1162 =
      _f(function (ts$1163) {
           if (_(oc$List$[19], [ is_constant$1041, ts$1163 ])) return never$1141;
           var match$1323 = _(make_event$1140, [ 0 ]);
           _(include$1290[66],
             [
               $(false),
               ts$1163,
               _f(function (param$1320) {
                    var loop$1166 =
                      _f(function (param$1321) {
                           if (param$1321) {
                             {
                               var r$1169 = _(read_result$1053, [ param$1321[0] ]);
                               switch ($t(r$1169))
                               {
                               case 1: if (r$1169[0][0] === Unset$1032) return __(loop$1166, [ param$1321[1] ]); return r$1169;
                               default: return r$1169;
                               }
                             }
                           }
                           throw $(Assert_failure$26g, $("froc.ml", 121, 16));
                         });
                    return __(write_temp_result$1117, [ match$1323[1], _(loop$1166, [ ts$1163 ]) ]);
                  })
             ]);
           return match$1323[0];
         });
    var map$1170 =
      _f(function (f$1171, t$1172) {
           if (_(is_constant$1041, [ t$1172 ])) return never$1141;
           var match$1319 = _(make_event$1140, [ 0 ]);
           _(notify_result_e$1146,
             [
               t$1172,
               _f(function (r$1175) {
                    var r$1176 =
                      function () {
                        switch ($t(r$1175))
                        {
                        case 0: try { return $(_(f$1171, [ r$1175[0] ])); } catch (e$1179) { return $1(e$1179); } break;
                        case 1: return $1(r$1175[0]);
                        default: return null;
                        }
                      }();
                    return __(write_temp_result$1117, [ match$1319[1], r$1176 ]);
                  })
             ]);
           return match$1319[0];
         });
    var map2$1180 =
      _f(function (f$1181, t1$1182, t2$1183) {
           if (_(is_constant$1041, [ t1$1182 ]) && _(is_constant$1041, [ t2$1183 ])) return never$1141;
           var match$1318 = _(make_event$1140, [ 0 ]);
           _(include$1290[42],
             [
               $(false),
               t1$1182,
               t2$1183,
               _f(function (param$1309) {
                    var r$1186 =
                      function () {
                        var match$1310 = _(read_result$1053, [ t1$1182 ]);
                        var match$1311 = _(read_result$1053, [ t2$1183 ]);
                        var $r45_0 = null;
                        var $r45 = false;
                        r$45: {
                          {
                            var $r44 = false;
                            r$44: {
                              {
                                var $r47 = false;
                                r$47: {
                                  {
                                    var $r48 = false;
                                    r$48:
                                      switch ($t(match$1310))
                                      {
                                      case 0:
                                        switch ($t(match$1311))
                                        {
                                        case 0:
                                          try {
                                            return $($(_(f$1181, [ match$1310[0], match$1311[0] ])));
                                          }
                                          catch (e$1191) {
                                            return $($1(e$1191));
                                          }
                                          break;
                                        default: $r48 = true; break r$48;
                                        }
                                        break;
                                      case 1:
                                        if (!(match$1310[0][0] === Unset$1032)) { { $r48 = true; break r$48; } }
                                        $r44 = true;
                                        break r$44;
                                      default: return null;
                                      }
                                    if ($r48)
                                      switch ($t(match$1311))
                                      {
                                      case 1:
                                        if (!(match$1311[0][0] === Unset$1032)) { { $r47 = true; break r$47; } }
                                        $r44 = true;
                                        break r$44;
                                      default: $r47 = true; break r$47;
                                      }
                                  }
                                }
                                if ($r47)
                                  switch ($t(match$1310))
                                  {
                                  case 1: $r45_0 = match$1310[0]; $r45 = true; break r$45;
                                  default: $r45_0 = match$1311[0]; $r45 = true; break r$45;
                                  }
                              }
                            }
                            if ($r44) return 0;
                          }
                        }
                        if ($r45) { { var e$1187 = $r45_0; return $($1(e$1187)); } }
                      }();
                    if (r$1186) return __(write_temp_result$1117, [ match$1318[1], r$1186[0] ]);
                    return 0;
                  })
             ]);
           return match$1318[0];
         });
    var filter$1193 =
      _f(function (p$1194, t$1195) {
           if (_(is_constant$1041, [ t$1195 ])) return never$1141;
           var match$1308 = _(make_event$1140, [ 0 ]);
           _(notify_result_e$1146,
             [
               t$1195,
               _f(function (r$1198) {
                    var r$1199 =
                      function () {
                        switch ($t(r$1198))
                        {
                        case 0:
                          var v$1200 = r$1198[0];
                          try {
                            if (_(p$1194, [ v$1200 ])) return $($(v$1200));
                            return 0;
                          }
                          catch (e$1201) {
                            return $($1(e$1201));
                          }
                          break;
                        case 1: return $(r$1198);
                        default: return null;
                        }
                      }();
                    if (r$1199) return __(write_temp_result$1117, [ match$1308[1], r$1199[0] ]);
                    return 0;
                  })
             ]);
           return match$1308[0];
         });
    var collect_e$1203 =
      _f(function (f$1204, init$1205, t$1206) {
           if (_(is_constant$1041, [ t$1206 ])) return never$1141;
           var match$1306 = _(make_event$1140, [ 0 ]);
           var st$1209 = $($(init$1205));
           _(notify_result_e$1146,
             [
               t$1206,
               _f(function (r$1210) {
                    var r$1211 =
                      function () {
                        var match$1302 = st$1209[0];
                        switch ($t(match$1302))
                        {
                        case 0:
                          switch ($t(r$1210))
                          {
                          case 0:
                            try { return $($(_(f$1204, [ match$1302[0], r$1210[0] ]))); } catch (e$1215) { return $($1(e$1215)); }
                            break;
                          default: return $($1(r$1210[0]));
                          }
                          break;
                        case 1: return 0;
                        default: return null;
                        }
                      }();
                    if (r$1211) {
                      {
                        var r$1216 = r$1211[0];
                        st$1209[0] = r$1216;
                        return __(write_temp_result$1117, [ match$1306[1], r$1216 ]);
                      }
                    }
                    return 0;
                  })
             ]);
           return match$1306[0];
         });
    var join_e$1217 =
      _f(function (ee$1218) {
           if (_(is_constant$1041, [ ee$1218 ])) return never$1141;
           var match$1301 = _(make_event$1140, [ 0 ]);
           var ru$1220 = match$1301[1];
           _(notify_result_e$1146,
             [
               ee$1218,
               _f(function (param$1300) {
                    switch ($t(param$1300))
                    {
                    case 0: return __(notify_result_e$1146, [ param$1300[0], _(write_temp_result$1117, [ ru$1220 ]) ]);
                    case 1: return __(write_temp_result$1117, [ ru$1220, $1(param$1300[0]) ]);
                    default: return null;
                    }
                  })
             ]);
           return match$1301[0];
         });
    var fix_e$1223 =
      _f(function (ef$1224) {
           var match$1299 = _(make_event$1140, [ 0 ]);
           var e$1227 = _(ef$1224, [ match$1299[0] ]);
           _(notify_result_e$1146, [ e$1227, _(send_result_deferred$1121, [ match$1299[1] ]) ]);
           return e$1227;
         });
    var join_b$1236 =
      _f(function (eq$1237, bb$1238) { return __(bind$1042, [ eq$1237, bb$1238, _f(function (b$1239) { return b$1239; }) ]); });
    var fix_b$1240 =
      _f(function (eq$1241, bf$1242) {
           var match$1298 = _(make_changeable$1065, [ eq$1241, 0, 0 ]);
           var b$1245 = _(bf$1242, [ match$1298[0] ]);
           _(notify_result$1060,
             [
               0,
               b$1245,
               _f(function (r$1246) {
                    _(oc$Queue$[2],
                      [ _f(function (param$1297) { return __(write_result$1056, [ match$1298[1], r$1246 ]); }), q$1108 ]);
                    return __(run_queue$1112, [ 0 ]);
                  })
             ]);
           return b$1245;
         });
    var switch$1247 =
      _f(function (eq$1248, b$1249, e$1250) {
           if (_(is_constant$1041, [ e$1250 ])) return b$1249;
           var match$1296 = _(make_changeable$1065, [ eq$1248, 0, 0 ]);
           var bu$1252 = match$1296[1];
           _(notify_result$1060,
             [
               0,
               e$1250,
               _f(function (param$1295) {
                    switch ($t(param$1295))
                    {
                    case 0: return __(connect$1062, [ bu$1252, param$1295[0] ]);
                    case 1:
                      var e$1254 = param$1295[0];
                      if (e$1254[0] === Unset$1032) return __(connect$1062, [ bu$1252, b$1249 ]);
                      return __(include$1290[21], [ bu$1252, e$1254 ]);
                    default: return null;
                    }
                  })
             ]);
           return match$1296[0];
         });
    var until$1255 =
      _f(function (eq$1256, b$1257, e$1258) { return __(switch$1247, [ eq$1256, b$1257, _(next$1156, [ e$1258 ]) ]); });
    var hold_result$1259 =
      _f(function (eq$1260, init$1261, e$1262) {
           if (_(is_constant$1041, [ e$1262 ])) return __(make_constant$1066, [ init$1261 ]);
           var match$1294 = _(make_changeable$1065, [ eq$1260, $(init$1261), 0 ]);
           _(notify_result_e$1146, [ e$1262, _(write_result$1056, [ match$1294[1] ]) ]);
           return match$1294[0];
         });
    var hold$1265 = _f(function (eq$1266, init$1267, e$1268) { return __(hold_result$1259, [ eq$1266, $(init$1267), e$1268 ]); });
    var collect_b$1269 =
      _f(function (f$1270, init$1271, t$1272) {
           return __(hold$1265, [ 0, init$1271, _(collect_e$1203, [ f$1270, init$1271, t$1272 ]) ]);
         });
    var changes$1273 =
      _f(function (b$1274) {
           if (_(is_constant$1041, [ b$1274 ])) return never$1141;
           var match$1293 = _(make_event$1140, [ 0 ]);
           _(notify_result$1060, [ $(false), b$1274, _(write_temp_result$1117, [ match$1293[1] ]) ]);
           return match$1293[0];
         });
    var when_true$1277 =
      _f(function (b$1278) {
           return __(map$1170,
                     [
                       _f(function (b$1279) { return 0; }),
                       _(filter$1193, [ _f(function (b$1280) { return b$1280; }), _(changes$1273, [ b$1278 ]) ])
                     ]);
         });
    var count$1281 =
      _f(function (t$1282) { return __(collect_b$1269, [ _f(function (n$1283, param$1292) { return n$1283 + 1; }), 0, t$1282 ]); });
    var make_cell$1284 =
      _f(function (v$1285) {
           var match$1291 = _(make_event$1140, [ 0 ]);
           return $(_(hold$1265, [ 0, v$1285, match$1291[0] ]), _(send_deferred$1133, [ match$1291[1] ]));
         });
    return $(include$1290[5], include$1290[6], bind$1042, include$1290[9], 
             include$1290[11], include$1290[10], include$1290[18], read_result$1053, 
             include$1290[14], include$1290[16], include$1290[15], include$1290[17], join_b$1236, fix_b$1240, notify$1058,
             notify_cancel$1059, notify_result$1060, notify_result_cancel$1061, hash$1067, make_event$1140, never$1141,
             notify_e$1152, notify_e_cancel$1149, notify_result_e$1146, notify_result_e_cancel$1143, send$1127, send_exn$1130,
             send_result$1120, send_deferred$1133, send_exn_deferred$1136, send_result_deferred$1121, next$1156, merge$1162,
             map$1170, map2$1180, filter$1193, collect_e$1203, collect_b$1269, join_e$1217, fix_e$1223, hash$1067, switch$1247,
             until$1255, hold$1265, hold_result$1259, changes$1273, when_true$1277, count$1281, make_cell$1284, init$1111,
             no_cancel$1035, cancel$1036, include$1290[30], include$1290[38], 
             include$1290[36], set_debug$1104, include$1290[39], include$1290[41], 
             include$1290[40], include$1290[43], include$1290[45], include$1290[44], 
             include$1290[47], include$1290[49], include$1290[48], include$1290[51], 
             include$1290[53], include$1290[52], include$1290[55], include$1290[57], 
             include$1290[56], include$1290[59], include$1290[61], include$1290[60], 
             include$1290[63], include$1290[65], include$1290[64]);
  }();
var oc$Ocamljs$ =
  function () {
    var option_of_nullable$1046 = _f(function (x$1047) { if (x$1047 === null) return 0; return $(x$1047); });
    var nullable_of_option$1048 = _f(function (x$1049) { if (x$1049) return x$1049[0]; return null; });
    var is_null$1051 = _f(function (a$1052) { return caml_equal(a$1052, null); });
    var Inline$1240 = function () { var Jslib_ast$1234 = $(); var _loc$1239 = 0; return $(Jslib_ast$1234, _loc$1239); }();
    return $(option_of_nullable$1046, nullable_of_option$1048, is_null$1051, Inline$1240);
  }();
var oc$Javascript$ =
  function () {
    var typeof$1050 = _f(function (o$1051) { return typeof o$1051; });
    var true_$1052 = true;
    var false_$1053 = false;
    var new_Date$1091 = _f(function (param$1119) { return new Date(); });
    var Js_string$1116 = $();
    var Math$1118 = function () { var pi$1117 = Math.PI; return $(pi$1117); }();
    return $(typeof$1050, true_$1052, false_$1053, new_Date$1091, Js_string$1116, Math$1118);
  }();
var oc$Dom$ = function () { var window$1706 = window; var document$1707 = document; return $(window$1706, document$1707); }();
var oc$Froc_dom$ =
  function () {
    var $7C$3E$1030 = _f(function (x$1031, f$1032) { return __(f$1032, [ x$1031 ]); });
    var ticks_b$1035 =
      _f(function (msb$1036) {
           var match$1250 = _(oc$Froc$[19], [ 0 ]);
           var id$1039 = $(0);
           var clear$1040 =
             _f(function (param$1248) {
                  var match$1249 = id$1039[0];
                  if (match$1249) {
                    {
                      (function () { var v$1259 = oc$Dom$[0]; return _m(v$1259.clearInterval, v$1259, [ match$1249[0] ]); }());
                      return id$1039[0] = 0;
                    }
                  }
                  return 0;
                });
           var set_interval$1042 =
             _f(function (r$1043) {
                  _(clear$1040, [ 0 ]);
                  switch ($t(r$1043))
                  {
                  case 0:
                    return id$1039[0] =
                             $(function () {
                                 var v$1258 = oc$Dom$[0];
                                 return _m(v$1258.setInterval, v$1258,
                                           [
                                             _f(function (param$1246) { return __(oc$Froc$[25], [ match$1250[1], 0 ]); }),
                                             r$1043[0]
                                           ]);
                               }());
                  case 1: return 0;
                  default: return null;
                  }
                });
           _(oc$Froc$[52], [ clear$1040 ]);
           _(oc$Froc$[16], [ 0, msb$1036, set_interval$1042 ]);
           return match$1250[0];
         });
    var ticks$1045 =
      _f(function (ms$1046) {
           var match$1245 = _(oc$Froc$[19], [ 0 ]);
           var id$1049 =
             function () {
               var v$1257 = oc$Dom$[0];
               return _m(v$1257.setInterval, v$1257,
                         [ _f(function (param$1244) { return __(oc$Froc$[25], [ match$1245[1], 0 ]); }), ms$1046 ]);
             }();
           _(oc$Froc$[52],
             [
               _f(function (param$1243) {
                    return function () { var v$1256 = oc$Dom$[0]; return __m(v$1256.clearInterval, v$1256, [ id$1049 ]); }();
                  })
             ]);
           return match$1245[0];
         });
    var send_delayed_event$1059 =
      _f(function (e$1060, de$1061) {
           var send$1062 =
             _f(function (de$1063) {
                  if (!(de$1063[1] === de$1063)) {
                    {
                      _(oc$Froc$[30], [ e$1060, de$1063[0] ]);
                      var de_next$1064 = de$1063[1];
                      de$1063[1] = de$1063;
                      return __(send$1062, [ de_next$1064 ]);
                    }
                  }
                  return 0;
                });
           return __(send$1062, [ de$1061 ]);
         });
    var delay_eb$1065 =
      _f(function (t$1066, msb$1067) {
           var match$1242 = _(oc$Froc$[19], [ 0 ]);
           var s$1069 = match$1242[1];
           var de$1070 = $($1($(oc$Pervasives$[2])), de$1070);
           de$1070[1] = de$1070;
           var de_next$1071 = $(de$1070);
           _(oc$Froc$[23],
             [
               t$1066,
               _f(function (r$1072) {
                    var r$1073 = _(oc$Froc$[7], [ msb$1067 ]);
                    switch ($t(r$1073))
                    {
                    case 0:
                      var de$1075 = $(r$1072, de_next$1071[0]);
                      de_next$1071[0] = de$1075;
                      (function () {
                         var v$1255 = oc$Dom$[0];
                         return _m(v$1255.setTimeout, v$1255,
                                   [
                                     _f(function (param$1238) { return __(send_delayed_event$1059, [ s$1069, de$1075 ]); }),
                                     r$1073[0]
                                   ]);
                       }());
                      return 0;
                    case 1:
                      de_next$1071[0] = $(r$1073, de_next$1071[0]);
                      return __(send_delayed_event$1059, [ s$1069, de_next$1071[0] ]);
                    default: return null;
                    }
                  })
             ]);
           return match$1242[0];
         });
    var delay_e$1076 = _f(function (t$1077, ms$1078) { return __(delay_eb$1065, [ t$1077, _(oc$Froc$[0], [ ms$1078 ]) ]); });
    var delay_bb$1079 =
      _f(function (t$1080, msb$1081) {
           return __($7C$3E$1030,
                     [
                       _($7C$3E$1030,
                         [
                           _($7C$3E$1030, [ t$1080, oc$Froc$[45] ]),
                           _f(function (e$1082) { return __(delay_eb$1065, [ e$1082, msb$1081 ]); })
                         ]),
                       _(oc$Froc$[44], [ 0, _(oc$Froc$[7], [ t$1080 ]) ])
                     ]);
         });
    var delay_b$1083 = _f(function (t$1084, ms$1085) { return __(delay_bb$1079, [ t$1084, _(oc$Froc$[0], [ ms$1085 ]) ]); });
    var mouse_e$1086 =
      _f(function (param$1234) {
           var match$1236 = _(oc$Froc$[19], [ 0 ]);
           var f$1089 = _f(function (me$1090) { return __(oc$Froc$[25], [ match$1236[1], $(me$1090.clientX, me$1090.clientY) ]); });
           (function () { var v$1254 = oc$Dom$[1]; return _m(v$1254.addEventListener, v$1254, [ "mousemove", f$1089, false ]); }());
           _(oc$Froc$[52],
             [
               _f(function (param$1235) {
                    return function () {
                             var v$1253 = oc$Dom$[1];
                             return __m(v$1253.removeEventListener, v$1253, [ "mousemove", f$1089, false ]);
                           }();
                  })
             ]);
           return match$1236[0];
         });
    var mouse_b$1091 = _f(function (param$1233) { return __(oc$Froc$[43], [ 0, $(0, 0), _(mouse_e$1086, [ 0 ]) ]); });
    var on_event_prop_e$1092 =
      _f(function (el$1093, ev$1094, pf$1095) {
           var match$1232 = _(oc$Froc$[19], [ 0 ]);
           var f$1098 = _f(function (param$1231) { return __(oc$Froc$[25], [ match$1232[1], _(pf$1095, [ el$1093 ]) ]); });
           _m(el$1093.addEventListener, el$1093, [ ev$1094, f$1098, false ]);
           _(oc$Froc$[52],
             [ _f(function (param$1230) { return __m(el$1093.removeEventListener, el$1093, [ ev$1094, f$1098, false ]); }) ]);
           return match$1232[0];
         });
    var on_event_prop_b$1099 =
      _f(function (el$1100, ev$1101, pf$1102) {
           return __(oc$Froc$[43], [ 0, _(pf$1102, [ el$1100 ]), _(on_event_prop_e$1092, [ el$1100, ev$1101, pf$1102 ]) ]);
         });
    var window_innerSize_e$1103 =
      _f(function (param$1229) {
           return __(on_event_prop_e$1092,
                     [ oc$Dom$[0], "resize", _f(function (w$1104) { return $(w$1104.innerWidth, w$1104.innerHeight); }) ]);
         });
    var window_innerSize_b$1105 =
      _f(function (param$1228) {
           return __(on_event_prop_b$1099,
                     [ oc$Dom$[0], "resize", _f(function (w$1106) { return $(w$1106.innerWidth, w$1106.innerHeight); }) ]);
         });
    var input_value_e$1107 =
      _f(function ($2Aopt$2A$1108, input$1111) {
           var event$1109 = $2Aopt$2A$1108 ? $2Aopt$2A$1108[0] : "change";
           return __(on_event_prop_e$1092, [ input$1111, event$1109, _f(function (i$1112) { return i$1112.value; }) ]);
         });
    var input_value_b$1113 =
      _f(function ($2Aopt$2A$1114, input$1117) {
           var event$1115 = $2Aopt$2A$1114 ? $2Aopt$2A$1114[0] : "change";
           return __(on_event_prop_b$1099, [ input$1117, event$1115, _f(function (i$1118) { return i$1118.value; }) ]);
         });
    var attach_innerHTML_e$1119 =
      _f(function (el$1120, e$1121) {
           return __(oc$Froc$[21], [ e$1121, _f(function (s$1122) { return el$1120.innerHTML = s$1122; }) ]);
         });
    var attach_innerHTML_b$1123 =
      _f(function (el$1124, b$1125) {
           return __(oc$Froc$[14], [ 0, b$1125, _f(function (s$1126) { return el$1124.innerHTML = s$1126; }) ]);
         });
    var attach_input_value_e$1127 =
      _f(function (i$1128, e$1129) {
           return __(oc$Froc$[21], [ e$1129, _f(function (v$1130) { return i$1128.value = v$1130; }) ]);
         });
    var attach_input_value_b$1131 =
      _f(function (i$1132, b$1133) {
           return __(oc$Froc$[14], [ 0, b$1133, _f(function (v$1134) { return i$1132.value = v$1134; }) ]);
         });
    var attach_backgroundColor_e$1135 =
      _f(function (el$1136, e$1137) {
           return __(oc$Froc$[21], [ e$1137, _f(function (v$1138) { return el$1136.style.backgroundColor = v$1138; }) ]);
         });
    var attach_backgroundColor_b$1139 =
      _f(function (el$1140, b$1141) {
           return __(oc$Froc$[14], [ 0, b$1141, _f(function (v$1142) { return el$1140.style.backgroundColor = v$1142; }) ]);
         });
    var attach_color_e$1143 =
      _f(function (el$1144, e$1145) {
           return __(oc$Froc$[21], [ e$1145, _f(function (v$1146) { return el$1144.style.color = v$1146; }) ]);
         });
    var attach_color_b$1147 =
      _f(function (el$1148, b$1149) {
           return __(oc$Froc$[14], [ 0, b$1149, _f(function (v$1150) { return el$1148.style.color = v$1150; }) ]);
         });
    var attach_display_e$1151 =
      _f(function (el$1152, e$1153) {
           return __(oc$Froc$[21], [ e$1153, _f(function (v$1154) { return el$1152.style.display = v$1154; }) ]);
         });
    var attach_display_b$1155 =
      _f(function (el$1156, b$1157) {
           return __(oc$Froc$[14], [ 0, b$1157, _f(function (v$1158) { return el$1156.style.display = v$1158; }) ]);
         });
    var attach_fontSize_e$1159 =
      _f(function (el$1160, e$1161) {
           return __(oc$Froc$[21], [ e$1161, _f(function (v$1162) { return el$1160.style.fontSize = v$1162; }) ]);
         });
    var attach_fontSize_b$1163 =
      _f(function (el$1164, b$1165) {
           return __(oc$Froc$[14], [ 0, b$1165, _f(function (v$1166) { return el$1164.style.fontSize = v$1166; }) ]);
         });
    var attach_disabled_e$1167 =
      _f(function (el$1168, e$1169) {
           return __(oc$Froc$[21], [ e$1169, _f(function (v$1170) { return el$1168.disabled = v$1170; }) ]);
         });
    var attach_disabled_b$1171 =
      _f(function (el$1172, b$1173) {
           return __(oc$Froc$[14], [ 0, b$1173, _f(function (v$1174) { return el$1172.disabled = v$1174; }) ]);
         });
    var node_of_result$1175 =
      _f(function (param$1227) {
           switch ($t(param$1227))
           {
           case 0: return param$1227[0];
           case 1:
             var s$1178 = function () { var v$1252 = oc$Dom$[1]; return _m(v$1252.createElement, v$1252, [ "span" ]); }();
             var t$1179 = function () { var v$1251 = oc$Dom$[1]; return _m(v$1251.createTextNode, v$1251, [ "exception" ]); }();
             _m(s$1178.appendChild, s$1178, [ t$1179 ]);
             return s$1178;
           default: return null;
           }
         });
    var appendChild$1180 =
      _f(function (n$1181, nb$1182) {
           var old$1184 = $(0);
           var update$1185 =
             _f(function (r$1186) {
                  var c$1187 = _(node_of_result$1175, [ r$1186 ]);
                  var match$1226 = old$1184[0];
                  if (match$1226)
                    _m(n$1181.replaceChild, n$1181, [ c$1187, match$1226[0] ]);
                  else
                    _m(n$1181.appendChild, n$1181, [ c$1187 ]);
                  return old$1184[0] = $(c$1187);
                });
           return __(oc$Froc$[16], [ 0, nb$1182, update$1185 ]);
         });
    var replaceNode$1189 =
      _f(function (n$1190, nb$1191) {
           var p$1193 = n$1190.parentNode;
           var old$1194 = $(n$1190);
           var update$1195 =
             _f(function (r$1196) {
                  var c$1197 = _(node_of_result$1175, [ r$1196 ]);
                  _m(p$1193.replaceChild, p$1193, [ c$1197, old$1194[0] ]);
                  return old$1194[0] = c$1197;
                });
           return __(oc$Froc$[16], [ 0, nb$1191, update$1195 ]);
         });
    var event$1198 =
      _f(function (name$1199, elem$1200) {
           var match$1225 = _(oc$Froc$[19], [ 0 ]);
           var f$1203 = _(oc$Froc$[25], [ match$1225[1] ]);
           _m(elem$1200.addEventListener, elem$1200, [ name$1199, f$1203, false ]);
           _(oc$Froc$[52],
             [ _f(function (param$1224) { return __m(elem$1200.removeEventListener, elem$1200, [ name$1199, f$1203, false ]); }) ]);
           return match$1225[0];
         });
    var mouseEvent$1204 =
      _f(function (name$1205, elem$1206) {
           var match$1223 = _(oc$Froc$[19], [ 0 ]);
           var f$1209 = _(oc$Froc$[25], [ match$1223[1] ]);
           _m(elem$1206.addEventListener, elem$1206, [ name$1205, f$1209, false ]);
           _(oc$Froc$[52],
             [ _f(function (param$1222) { return __m(elem$1206.removeEventListener, elem$1206, [ name$1205, f$1209, false ]); }) ]);
           return match$1223[0];
         });
    var keyEvent$1210 =
      _f(function (name$1211, elem$1212) {
           var match$1221 = _(oc$Froc$[19], [ 0 ]);
           var f$1215 = _(oc$Froc$[25], [ match$1221[1] ]);
           _m(elem$1212.addEventListener, elem$1212, [ name$1211, f$1215, false ]);
           _(oc$Froc$[52],
             [ _f(function (param$1220) { return __m(elem$1212.removeEventListener, elem$1212, [ name$1211, f$1215, false ]); }) ]);
           return match$1221[0];
         });
    var clicks$1216 = _f(function (elem$1217) { return __(mouseEvent$1204, [ "click", elem$1217 ]); });
    return $(ticks$1045, ticks_b$1035, delay_e$1076, delay_eb$1065, delay_b$1083, delay_bb$1079, mouse_e$1086, mouse_b$1091,
             window_innerSize_e$1103, window_innerSize_b$1105, input_value_e$1107, input_value_b$1113, attach_innerHTML_e$1119,
             attach_innerHTML_b$1123, attach_input_value_e$1127, attach_input_value_b$1131, attach_backgroundColor_e$1135,
             attach_backgroundColor_b$1139, attach_color_e$1143, attach_color_b$1147, attach_display_e$1151, attach_display_b$1155,
             attach_fontSize_e$1159, attach_fontSize_b$1163, attach_disabled_e$1167, attach_disabled_b$1171, appendChild$1180,
             replaceNode$1189, clicks$1216, event$1198, mouseEvent$1204, keyEvent$1210);
  }();
var oc$Froc_dom_anim$ =
  function () {
    var color$1033 =
      _f(function (a$1034, r$1035, g$1036, b$1037) {
           if (a$1034) return __(oc$Printf$[4], [ "rgba(%d,%d,%d,%d)", r$1035, g$1036, b$1037, a$1034[0] ]);
           return __(oc$Printf$[4], [ "rgb(%d,%d,%d)", r$1035, g$1036, b$1037 ]);
         });
    var fillRect$1039 =
      _f(function (param$1080, w$1042, h$1043, color$1044, ctx$1045) {
           ctx$1045.fillStyle = color$1044;
           return __m(ctx$1045.fillRect, ctx$1045, [ param$1080[0], param$1080[1], w$1042, h$1043 ]);
         });
    var strokeRect$1046 =
      _f(function (param$1079, w$1049, h$1050, color$1051, ctx$1052) {
           ctx$1052.strokeStyle = color$1051;
           return __m(ctx$1052.strokeRect, ctx$1052, [ param$1079[0], param$1079[1], w$1049, h$1050 ]);
         });
    var disk$1053 =
      _f(function (param$1078, radius$1056, color$1057, ctx$1058) {
           ctx$1058.fillStyle = color$1057;
           _m(ctx$1058.beginPath, ctx$1058, [  ]);
           _m(ctx$1058.arc, ctx$1058, [ param$1078[0], param$1078[1], radius$1056, 0., 2. * oc$Javascript$[5][0], true ]);
           return __m(ctx$1058.fill, ctx$1058, [  ]);
         });
    var filled_poly$1059 =
      _f(function (points$1060, color$1061, ctx$1062) {
           ctx$1062.fillStyle = color$1061;
           _m(ctx$1062.beginPath, ctx$1062, [  ]);
           _(oc$List$[9],
             [
               _f(function (param$1077) { return __m(ctx$1062.lineTo, ctx$1062, [ param$1077[0], param$1077[1] ]); }),
               points$1060
             ]);
           _m(ctx$1062.closePath, ctx$1062, [  ]);
           return __m(ctx$1062.fill, ctx$1062, [  ]);
         });
    var draw$1065 =
      _f(function (canvas$1066, instrs$1067) {
           var ctx$1068 = _m(canvas$1066.getContext, canvas$1066, [ "2d" ]);
           _m(ctx$1068.clearRect, ctx$1068, [ 0., 0., canvas$1066.width, canvas$1066.height ]);
           return __(oc$ListLabels$[9],
                     [
                       _f(function (f$1069) {
                            _m(ctx$1068.save, ctx$1068, [  ]);
                            _(f$1069, [ ctx$1068 ]);
                            _m(ctx$1068.closePath, ctx$1068, [  ]);
                            return __m(ctx$1068.restore, ctx$1068, [  ]);
                          }),
                       instrs$1067
                     ]);
         });
    var attach$1070 =
      _f(function (canvas$1071, instrsb$1072) {
           var notify$1073 =
             _f(function (param$1075) {
                  switch ($t(param$1075))
                  {
                  case 0: return __(draw$1065, [ canvas$1071, param$1075[0] ]);
                  case 1: return 0;
                  default: return null;
                  }
                });
           return __(oc$Froc$[16], [ 0, instrsb$1072, notify$1073 ]);
         });
    return $(color$1033, fillRect$1039, strokeRect$1046, disk$1053, filled_poly$1059, attach$1070);
  }();
var oc$Puzzle$ =
  function () {
    var D$1030 = oc$Dom$;
    var F$1031 = oc$Froc$;
    var Fd$1032 = oc$Froc_dom$;
    var Fda$1033 = oc$Froc_dom_anim$;
    var $3E$3E$3D$1034 = F$1031[3];
    var $7C$3E$1035 = _f(function (x$1036, f$1037) { return __(f$1037, [ x$1036 ]); });
    var console$1043 = console;
    var onload$1057 =
      _f(function (param$1359) {
           var canvas$1058 = function () { var v$1385 = D$1030[1]; return _m(v$1385.getElementById, v$1385, [ "canvas" ]); }();
           var click_canvas$1059 = function () { var v$1384 = D$1030[1]; return _m(v$1384.createElement, v$1384, [ "canvas" ]); }();
           var xy_of_drag$1060 =
             _f(function (e$1061, init$1062) {
                  return __($7C$3E$1035,
                            [
                              _($7C$3E$1035,
                                [
                                  _($7C$3E$1035,
                                    [
                                      e$1061,
                                      _(F$1031[33],
                                        [
                                          _f(function (e$1063) {
                                               var match$1380 = e$1063.type;
                                               var $r31 = false;
                                               r$31: {
                                                 {
                                                   if (oc$$sneq(match$1380, "mousedown")) { { $r31 = true; break r$31; } }
                                                   if (!!e$1063.shiftKey) { { $r31 = true; break r$31; } }
                                                   return __($7C$3E$1035,
                                                             [
                                                               _($7C$3E$1035,
                                                                 [
                                                                   _(Fd$1032[30], [ "mousemove", canvas$1058 ]),
                                                                   _(F$1031[36],
                                                                    [
                                                                    _f
                                                                    (function 
                                                                    (param$1375, e$1066) {
                                                                    var match$1376 = param$1375[0];
                                                                    var x$27$1067 = e$1066.clientX;
                                                                    var y$27$1068 = e$1066.clientY;
                                                                    return $
                                                                    ($(x$27$1067, y$27$1068),
                                                                    $(x$27$1067 - match$1376[0], y$27$1068 - match$1376[1]));
                                                                    }),
                                                                    $($(e$1063.clientX, e$1063.clientY), $(0, 0))
                                                                    ])
                                                                 ]),
                                                               _(F$1031[33],
                                                                 [ _f(function (param$1378) { return param$1378[1]; }) ])
                                                             ]);
                                                 }
                                               }
                                               if ($r31) return F$1031[20];
                                             })
                                        ])
                                    ]),
                                  F$1031[38]
                                ]),
                              _(F$1031[37],
                                [
                                  _f(function (param$1381, param$1382) {
                                       return $(param$1381[0] + param$1382[0], param$1381[1] + param$1382[1]);
                                     }),
                                  init$1062
                                ])
                            ]);
                });
           var size$1074 = _(Fd$1032[9], [ 0 ]);
           _(F$1031[14],
             [
               0,
               size$1074,
               _f(function (param$1374) {
                    var w$1077 = param$1374[0] - 2 * canvas$1058.offsetLeft;
                    var h$1078 = param$1374[1] - 2 * canvas$1058.offsetTop;
                    canvas$1058.width = w$1077;
                    canvas$1058.height = h$1078;
                    click_canvas$1059.width = w$1077;
                    return click_canvas$1059.height = h$1078;
                  })
             ]);
           var num_shapes$1079 = 25;
           var shape_events$1080 =
             _(oc$Array$[0], [ num_shapes$1079 + 1, _f(function (param$1373) { return __(F$1031[19], [ 0 ]); }) ]);
           var mouse_events$1081 =
             _(F$1031[32],
               [
                 _(oc$List$[10],
                   [
                     _f(function (t$1082) { return __(Fd$1032[30], [ t$1082, canvas$1058 ]); }),
                     $("mousedown", $("mouseup", $("mouseout", 0)))
                   ])
               ]);
           var last$1083 = $(0);
           _(F$1031[21],
             [
               mouse_events$1081,
               _f(function (e$1084) {
                    var match$1372 = e$1084.type;
                    if (oc$$sneq(match$1372, "mousedown")) {
                      {
                        var match$1370 = oc$$arefs(shape_events$1080, last$1083[0]);
                        _(F$1031[25], [ match$1370[1], e$1084 ]);
                        return last$1083[0] = 0;
                      }
                    }
                    var i$1085 =
                      function () {
                        var x$1086 = e$1084.clientX - canvas$1058.offsetLeft;
                        var y$1087 = e$1084.clientY - canvas$1058.offsetTop;
                        var id$1088 =
                          function () {
                            var v$1383 = _m(click_canvas$1059.getContext, click_canvas$1059, [ "2d" ]);
                            return _m(v$1383.getImageData, v$1383, [ x$1086, y$1087, 1., 1. ]);
                          }();
                        var d$1089 = id$1088.data;
                        return oc$$arefs(d$1089, 0);
                      }();
                    var match$1368 = oc$$arefs(shape_events$1080, i$1085);
                    _(F$1031[25], [ match$1368[1], e$1084 ]);
                    return last$1083[0] = i$1085;
                  })
             ]);
           var shapes$1092 =
             _(oc$Array$[0],
               [
                 num_shapes$1079,
                 _f(function (i$1093) {
                      var match$1366 = oc$$arefs(shape_events$1080, i$1093 + 1);
                      var e$1094 = match$1366[0];
                      var angle$1095 =
                        _($7C$3E$1035,
                          [
                            _($7C$3E$1035,
                              [
                                _($7C$3E$1035,
                                  [
                                    e$1094,
                                    _(F$1031[33],
                                      [
                                        _f(function (e$1096) {
                                             var match$1365 = e$1096.type;
                                             var $r9 = false;
                                             r$9: {
                                               {
                                                 if (oc$$sneq(match$1365, "mousedown")) { { $r9 = true; break r$9; } }
                                                 if (!e$1096.shiftKey) { { $r9 = true; break r$9; } }
                                                 var x$1097 = e$1096.clientX;
                                                 var y$1098 = e$1096.clientY;
                                                 return __($7C$3E$1035,
                                                           [
                                                             _(Fd$1032[30], [ "mousemove", canvas$1058 ]),
                                                             _(F$1031[33],
                                                               [
                                                                 _f(function 
                                                                    (e$1099) {
                                                                    var x$27$1100 = e$1099.clientX;
                                                                    var y$27$1101 = e$1099.clientY;
                                                                    return Math.atan2(x$27$1100 - x$1097, y$27$1101 - y$1098);
                                                                    })
                                                               ])
                                                           ]);
                                               }
                                             }
                                             if ($r9) return F$1031[20];
                                           })
                                      ])
                                  ]),
                                F$1031[38]
                              ]),
                            _(F$1031[43], [ 0, 0. ])
                          ]);
                      return $(i$1093 + 1, _(oc$Random$[4], [ 128 ]), 
                               _(oc$Random$[4], [ 128 ]),
                               _(Fda$1033[0],
                                 [
                                   $(_(oc$Random$[4], [ 256 ])),
                                   _(oc$Random$[4], [ 256 ]),
                                   _(oc$Random$[4], [ 256 ]),
                                   _(oc$Random$[4], [ 256 ])
                                 ]), _(xy_of_drag$1060, [ e$1094, $(_(oc$Random$[4], [ 1024 ]), _(oc$Random$[4], [ 512 ])) ]),
                               angle$1095);
                    })
               ]);
           var xy$1102 =
             function () {
               var match$1362 = oc$$arefs(shape_events$1080, 0);
               return _(xy_of_drag$1060, [ match$1362[0], $(0, 0) ]);
             }();
           var shapes$1104 =
             _f(function (color$1105) {
                  return __(F$1031[2],
                            [
                              0,
                              xy$1102,
                              _f(function (param$1360) {
                                   var py$1107 = param$1360[1];
                                   var px$1106 = param$1360[0];
                                   return __(F$1031[74],
                                             [
                                               0,
                                               _(oc$List$[10],
                                                 [
                                                   _f(function (s$1108) {
                                                        return __(F$1031[56],
                                                                  [
                                                                    0,
                                                                    s$1108[4],
                                                                    s$1108[5],
                                                                    _f
                                                                    (function 
                                                                    (param$1361, a$1111) {
                                                                    var y$1110 = param$1361[1];
                                                                    var x$1109 = param$1361[0];
                                                                    return __
                                                                    (F$1031[0],
                                                                    [
                                                                    _f
                                                                    (function 
                                                                    (ctx$1112) {
                                                                    _m
                                                                    (ctx$1112.translate, ctx$1112,
                                                                    [
                                                                    px$1106 + x$1109 + (s$1108[1] / 2 >> 0),
                                                                    py$1107 + y$1110 + (s$1108[2] / 2 >> 0)
                                                                    ]);
                                                                    _m(ctx$1112.rotate, ctx$1112, [ -a$1111 ]);
                                                                    _m
                                                                    (ctx$1112.translate, ctx$1112,
                                                                    [
                                                                    -px$1106 - x$1109 - (s$1108[1] / 2 >> 0),
                                                                    -py$1107 - y$1110 - (s$1108[2] / 2 >> 0)
                                                                    ]);
                                                                    return __
                                                                    (Fda$1033[1],
                                                                    [
                                                                    $(px$1106 + x$1109, py$1107 + y$1110),
                                                                    s$1108[1],
                                                                    s$1108[2],
                                                                    _(color$1105, [ s$1108 ]),
                                                                    ctx$1112
                                                                    ]);
                                                                    })
                                                                    ]);
                                                                    })
                                                                  ]);
                                                      }),
                                                   _(oc$Array$[9], [ shapes$1092 ])
                                                 ]),
                                               F$1031[0]
                                             ]);
                                 })
                            ]);
                });
           _(Fda$1033[5], [ canvas$1058, _(shapes$1104, [ _f(function (s$1113) { return s$1113[3]; }) ]) ]);
           return __(Fda$1033[5],
                     [
                       click_canvas$1059,
                       _(shapes$1104, [ _f(function (s$1114) { return __(Fda$1033[0], [ 0, s$1114[0], 0, 0 ]); }) ])
                     ]);
         });
    (D$1030[0]).onload = onload$1057;
    return $(D$1030, F$1031, Fd$1032, Fda$1033, $3E$3E$3D$1034, $7C$3E$1035, console$1043, onload$1057);
  }();
var oc$Std_exit$ = (_(oc$Pervasives$[80], [ 0 ]), $());
return caml_named_value;
})();
