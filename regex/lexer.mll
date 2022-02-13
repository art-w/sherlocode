{
  open Parser
}

rule token = parse
| "\\|"  { OR }
| "\\("  { PAR_OPEN }
| "\\)"  { PAR_CLOSE }
| "\\*"  { STAR }
| "\\+"  { PLUS }
| "\\?"  { OPTIONAL }
| "\\."  { ANY }
| "-"    { RANGE }
| "\\["  { RANGE_START }
| "\\[^" { RANGE_EXCL_START }
| "\\]"  { RANGE_END }
| "\\^"  { LINE_START }
| "\\$"  { LINE_END }
| "\\b"  { WORD_BOUNDARY }
| "\\w"  { ALPHANUM }
| "\\\\" { CHAR '\\' }
| _ as s { CHAR s }
| eof    { EOF }
