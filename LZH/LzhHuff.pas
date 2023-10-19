unit LzhHuff;
{$mode objfpc}

{$A+,B-,D+,E-,F-,I+,L+,N-,O+,R-,S-,V-}
// LZSS compression and adaptive Huffman coding
// used in .LZH archives as "LH1" compression method
// Based on code by Haruhiko OKUMURA and Haruyasu YOSHIZAKI 29-NOV-1988
// Edited and translated to English by Kenji RIKITAKE
// Translated from C to Turbo Pascal by Douglas Webb        18-FEB-1991
// Modified and ported to Lazarus: github.com/PascalVault   12-OCT-2023

interface

uses Classes;

const
  EXIT_OK = 0;
  EXIT_FAILED = 1;

  //LZSS Parameters
  N  = 4096;  // Size of string buffer
  F  = 60;    // Size of look-ahead buffer
  THRESHOLD = 2;
  NUL  = N;   // End of tree's node
  N_CHAR   = (256 - THRESHOLD + F);

  //character code (:= 0..N_CHAR-1)
  T     = (N_CHAR * 2 - 1);   // Size of table
  R     = (T - 1);            // root position
  MAX_FREQ = $8000;

  // update when cumulative frequency
  // reaches to this value

type
  Freqtype = array[0..T] OF UInt16;
  FreqPtr = ^freqtype;
  PntrType = array[0..T+N_Char] OF Int16;
  pntrPtr = ^pntrType;
  SonType = array[0..T] OF Int16;
  SonPtr = ^SonType;

  TextBufType = array[0..N+F-1] OF BYTE;
  TBufPtr = ^TextBufType;
  UInt16Ray = array[0..N+1] OF Int16;
  UInt16RayPtr = ^UInt16Ray;
  BUInt16Ray = array[0..N+257] OF Int16;
  BUInt16RayPtr = ^BUInt16Ray;


const
   // Tables for encoding/decoding upper 6 bits of
   // sliding dictionary pointer

   // encoder table
   p_len : array[0..63] of BYTE = (
   $03, $04, $04, $04, $05, $05, $05, $05,
   $05, $05, $05, $05, $06, $06, $06, $06,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $08, $08, $08, $08, $08, $08, $08, $08,
   $08, $08, $08, $08, $08, $08, $08, $08);

   p_code : array [0..63] OF BYTE = (
   $00, $20, $30, $40, $50, $58, $60, $68,
   $70, $78, $80, $88, $90, $94, $98, $9C,
   $A0, $A4, $A8, $AC, $B0, $B4, $B8, $BC,
   $C0, $C2, $C4, $C6, $C8, $CA, $CC, $CE,
   $D0, $D2, $D4, $D6, $D8, $DA, $DC, $DE,
   $E0, $E2, $E4, $E6, $E8, $EA, $EC, $EE,
   $F0, $F1, $F2, $F3, $F4, $F5, $F6, $F7,
   $F8, $F9, $FA, $FB, $FC, $FD, $FE, $FF);

   // decoder table
   d_code: array [0..255] OF BYTE = (
   $00, $00, $00, $00, $00, $00, $00, $00,
   $00, $00, $00, $00, $00, $00, $00, $00,
   $00, $00, $00, $00, $00, $00, $00, $00,
   $00, $00, $00, $00, $00, $00, $00, $00,
   $01, $01, $01, $01, $01, $01, $01, $01,
   $01, $01, $01, $01, $01, $01, $01, $01,
   $02, $02, $02, $02, $02, $02, $02, $02,
   $02, $02, $02, $02, $02, $02, $02, $02,
   $03, $03, $03, $03, $03, $03, $03, $03,
   $03, $03, $03, $03, $03, $03, $03, $03,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $08, $08, $08, $08, $08, $08, $08, $08,
   $09, $09, $09, $09, $09, $09, $09, $09,
   $0A, $0A, $0A, $0A, $0A, $0A, $0A, $0A,
   $0B, $0B, $0B, $0B, $0B, $0B, $0B, $0B,
   $0C, $0C, $0C, $0C, $0D, $0D, $0D, $0D,
   $0E, $0E, $0E, $0E, $0F, $0F, $0F, $0F,
   $10, $10, $10, $10, $11, $11, $11, $11,
   $12, $12, $12, $12, $13, $13, $13, $13,
   $14, $14, $14, $14, $15, $15, $15, $15,
   $16, $16, $16, $16, $17, $17, $17, $17,
   $18, $18, $19, $19, $1A, $1A, $1B, $1B,
   $1C, $1C, $1D, $1D, $1E, $1E, $1F, $1F,
   $20, $20, $21, $21, $22, $22, $23, $23,
   $24, $24, $25, $25, $26, $26, $27, $27,
   $28, $28, $29, $29, $2A, $2A, $2B, $2B,
   $2C, $2C, $2D, $2D, $2E, $2E, $2F, $2F,
   $30, $31, $32, $33, $34, $35, $36, $37,
   $38, $39, $3A, $3B, $3C, $3D, $3E, $3F);

   d_len: array[0..255] of BYTE = (
   $03, $03, $03, $03, $03, $03, $03, $03,
   $03, $03, $03, $03, $03, $03, $03, $03,
   $03, $03, $03, $03, $03, $03, $03, $03,
   $03, $03, $03, $03, $03, $03, $03, $03,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $08, $08, $08, $08, $08, $08, $08, $08,
   $08, $08, $08, $08, $08, $08, $08, $08);

   getbuf : UInt16 = 0;
   getlen : BYTE = 0;
   putlen : BYTE = 0;
   putbuf : UInt16 = 0;
   textsize : Int32 = 0;
   codesize : Int32 = 0;
   printcount : Int32 = 0;
   match_position : Int16 = 0;
   match_length : Int16 = 0;


  { TLZH }
  type
  TLZH = class
  private
    code, len : UInt16;
    text_buf : TBufPtr;
    lson,dad : UInt16RayPtr;
    rson : BUInt16RayPtr;
    freq : FreqPtr;  //cumulative freq table
    //pointing parent nodes.
    //area [T..(T + N_CHAR - 1)] are pointers for leaves
    prnt : PntrPtr;
    //pointing children nodes (son[], son[] + 1)
    son : SonPtr;

    FInStream: TStream;
    FOutStream: TStream;
    procedure InitTree;
    procedure InsertNode(r : Int16);
    procedure DeleteNode(p: Int16);
    function GetBit: Int16;
    function GetByte: Int16;
    procedure Putcode(l : Int16; c: UInt16);
    procedure StartHuff;
    procedure reconst;
    procedure update(c : Int16);
    procedure EncodeChar(c: UInt16);
    procedure EncodePosition(c : UInt16);
    procedure EncodeEnd;
    function DecodeChar: Int16;
    function DecodePosition : UInt16;
    procedure InitLZH;
    procedure EndLZH;
    procedure PutBytes(var Data; NumBytes:UInt16; var Bytes_Put : UInt16);
    procedure GetBytes(var Data; NumBytes:UInt16; var Bytes_Got : UInt16);
  public
    procedure Encode(var Bytes_Written:Int32);
    procedure Decode(UnpackedSize : Int32);
    constructor Create(InStream, OutStream: TStream); overload;
  end;

implementation

procedure TLZH.InitTree;  // Initializing tree
var i: Int16;
begin
  for i := N + 1 TO N + 256 do
    rson^[i] := NUL;   // root
  for i := 0 TO N do
    dad^[i] := NUL;    // node
end;

procedure TLZH.InsertNode(r : Int16);  // Inserting node to the tree
var tmp,i, p, cmp : Int16;
    key : TBufPtr;
    c : UInt16;
begin
  cmp := 1;
  key := @text_buf^[r];
  p := SUCC(N) + key^[0];
  rson^[r] := NUL;
  lson^[r] := NUL;
  match_length := 0;

  while match_length < F do begin
    if (cmp >= 0) then begin
      if (rson^[p] <> NUL) then
         p := rson^[p]
      else begin
        rson^[p] := r;
        dad^[r] := p;
         exit;
      end;
    end
    else begin
      if (lson^[p] <> NUL) then
       p := lson^[p]
    else begin
      lson^[p] := r;
      dad^[r] := p;
      exit;
    end;
  end;
  i := 0;
  cmp := 0;

  while (i < F) and (cmp = 0) do begin
   inc(i);
   cmp := key^[i] - text_buf^[p + i];
  end;

  if (i > THRESHOLD) then begin
    tmp := Pred((r - p) and Pred(N));

    if (i > match_length) then begin
      match_position := tmp;
      match_length := i;
    end;

    if (match_length < F) and (i = match_length) then begin
      c := tmp;
      if (c < match_position) then match_position := c;
      end;
    end;
  end;                // while TRUE do
  dad^[r] := dad^[p];
  lson^[r] := lson^[p];
  rson^[r] := rson^[p];
  dad^[lson^[p]] := r;
  dad^[rson^[p]] := r;

  if (rson^[dad^[p]] = p) then rson^[dad^[p]] := r
  else                         lson^[dad^[p]] := r;
  dad^[p] := NUL;  // remove p
end;

procedure TLZH.DeleteNode(p: Int16);  { Deleting node from the tree }
var q : Int16;
begin
  if (dad^[p] = NUL)       then exit;   // unregistered
  if (rson^[p] = NUL)      then q := lson^[p]
  else if (lson^[p] = NUL) then q := rson^[p]
  else begin
    q := lson^[p];
    if (rson^[q] <> NUL) then begin
      repeat
        q := rson^[q];
      until (rson^[q] = NUL);

      rson^[dad^[q]] := lson^[q];
      dad^[lson^[q]] := dad^[q];
      lson^[q] := lson^[p];
      dad^[lson^[p]] := q;
     end;

    rson^[q] := rson^[p];
    dad^[rson^[p]] := q;
  end;

  dad^[q] := dad^[p];
  if (rson^[dad^[p]] = p) then rson^[dad^[p]] := q
  else                         lson^[dad^[p]] := q;

  dad^[p] := NUL;
end;


// Huffman coding parameters
function TLZH.GetBit: Int16; // get one bit
var i: BYTE;
    i2 : Int16;
    res : UInt16;
begin
  while (getlen <= 8) do begin
    GetBytes(i,1,Res);
    if Res = 1 then i2 := i
    else            i2 := 0;
    getbuf := getbuf OR (i2 SHL (8 - getlen));
    Inc(getlen,8);
  end;

  i2 := getbuf;
  getbuf := getbuf SHL 1;
  Dec(getlen);
  getbit := Int16((i2 < 0));
end;

function TLZH.GetByte: Int16; // get a byte
var j : BYTE;
    i, res : UInt16;
begin
  while (getlen <= 8) do begin
    GetBytes(j,1,res);
    if Res = 1 then i := j
    else            i := 0;
    getbuf := getbuf OR (i SHL (8 - getlen));
    Inc(getlen,8);
  end;

  i := getbuf;
  getbuf := getbuf SHL 8;
  Dec(getlen,8);
  getbyte := Int16(i SHR 8);
end;

procedure TLZH.Putcode(l : Int16; c: UInt16);  // output c bits
var Temp : BYTE;
    Got : UInt16;
begin
  putbuf := putbuf OR (c SHR putlen);
  inc(putlen,l);
  if (putlen >= 8) then begin
    Temp := putbuf SHR 8;
    PutBytes(Temp,1,Got);
    DEC(putlen,8);
    if (putlen  >= 8) then begin
      Temp := Lo(PutBuf);
      PutBytes(Temp,1,Got);
      Inc(codesize,2);
      DEC(putlen,8);
      putbuf := c SHL (l - putlen);
    end
    else begin
      putbuf := putbuf SHL 8;
     Inc(codesize);
   end;
 end;
end;

//initialize freq tree
procedure TLZH.StartHuff;
var i, j : Int16;
begin
  for i := 0 to Pred(N_CHAR) do begin
    freq^[i] := 1;
    son^[i] := i + T;
    prnt^[i + T] := i;
  end;

  i := 0;
  j := N_CHAR;
  while (j <= R) do begin
    freq^[j] := freq^[i] + freq^[i + 1];
    son^[j] := i;
    prnt^[i] := j;
    prnt^[i + 1] := j;
    Inc(i,2);
    Inc(j);
  end;
  freq^[T] := $ffff;
  prnt^[R] := 0;
end;

// reconstruct freq tree
procedure TLZH.reconst;
var i, j, k, tmp : Int16;
    f, l : UInt16;
begin
 // halven cumulative freq for leaf nodes
  j := 0;
  for i := 0 to Pred(T) do begin
    if (son^[i] >= T) then begin
      freq^[j] := SUCC(freq^[i]) MOD 2;
      son^[j] := son^[i];
      Inc(j);
    end;
  end;
  // make a tree : first, connect children nodes
  i := 0;
  j := N_CHAR;
  while (j < T) do begin
    k := SUCC(i);
    f := freq^[i] + freq^[k];
    freq^[j] := f;
    k := Pred(j);
    while f < freq^[k] do DEC(K);
    Inc(k);
    l := (j - k) SHL 1;
    tmp := SUCC(k);
    move(freq^[k], freq^[tmp], l);
    freq^[k] := f;
    move(son^[k], son^[tmp], l);
    son^[k] := i;
    Inc(i,2);
    Inc(j);
  end;
  // connect parent nodes
  for i := 0 to Pred(T) do begin
    k := son^[i];
    if (k >= T) then begin
      prnt^[k] := i;
    end
    else begin
      prnt^[k] := i;
      prnt^[SUCC(k)] := i;
    end;
  end;
end;

// update freq tree
procedure TLZH.update(c : Int16);
var i, j, k, l : Int16;
begin
  if (freq^[R] = MAX_FREQ) then begin
    reconst;
  end;
  c := prnt^[c + T];

  repeat
    Inc(freq^[c]);
    k := freq^[c];

    // swap nodes to keep the tree freq-ordered
    l := SUCC(C);
    if (k > freq^[l]) then begin
       while (k > freq^[l]) do Inc(l);
       DEC(l);
       freq^[c] := freq^[l];
       freq^[l] := k;

       i := son^[c];
       prnt^[i] := l;
       if (i < T) then prnt^[SUCC(i)] := l;

       j := son^[l];
       son^[l] := i;

       prnt^[j] := c;
       if (j < T) then prnt^[SUCC(j)] := c;
       son^[c] := j;

       c := l;
     end;
     c := prnt^[c];
   until (c = 0); // REPEAT it until reaching the root
end;

procedure TLZH.EncodeChar(c: UInt16);
var i : UInt16;
    j, k : Int16;
begin
  i := 0;
  j := 0;
  k := prnt^[c + T];

  // search connections from leaf node to the root
  repeat
    i := i SHR 1;

    // if node's address is odd, output 1
    // else output 0

    if BOOLEAN(k and 1) then Inc(i,$8000);
    Inc(j);
    k := prnt^[k];
  until (k = R);

  Putcode(j, i);
  code := i;
  len := j;
  update(c);
end;

procedure TLZH.EncodePosition(c : UInt16);
var i,j : UInt16;
begin
  // output upper 6 bits with encoding
  i := c SHR 6;
  j := p_code[i];
  Putcode(p_len[i],j SHL 8);

  // output lower 6 bits directly
  Putcode(6, (c and $3f) SHL 10);
end;

procedure TLZH.EncodeEnd;
var Temp : BYTE;
    Got : UInt16;
begin
  if BOOLEAN(putlen) then begin
    Temp := Lo(putbuf SHR 8);
    PutBytes(Temp,1,Got);
    Inc(codesize);
  end;
end;

function TLZH.DecodeChar: Int16;
var c : UInt16;
begin
  c := son^[R];

  // start searching tree from the root to leaves.
  // choose node #(son[]) if input bit = 0
  // else choose #(son[]+1) (input bit = 1)
  while (c < T) do begin
    c := c + GetBit;
    c := son^[c];
  end;

  c := c - T;
  update(c);
  Decodechar := Int16(c);
end;

function TLZH.DecodePosition : UInt16;
var i, j, c : UInt16;
begin
  //decode upper 6 bits from given table
  i := GetByte;
  c := UInt16(d_code[i] SHL 6);
  j := d_len[i];

  //input lower 6 bits directly
  DEC(j,2);
  While j <> 0 do begin
    i := UInt16( (i SHL 1) + GetBit );
    DEC(J);
  end;

  DecodePosition := c OR i and $3f;
end;

// Compression
procedure TLZH.InitLZH;
begin
  getbuf := 0;
  getlen := 0;
  putlen := 0;
  putbuf := 0;
  textsize := 0;
  codesize := 0;
  printcount := 0;
  match_position := 0;
  match_length := 0;
  New(lson);
  New(dad);
  New(rson);
  New(text_buf);
  New(freq);
  New(prnt);
  New(son);
end;

procedure TLZH.EndLZH;
begin
  Dispose(son);
  Dispose(prnt);
  Dispose(freq);
  Dispose(text_buf);
  Dispose(rson);
  Dispose(dad);
  Dispose(lson);
end;

procedure TLZH.PutBytes(var Data; NumBytes: UInt16; var Bytes_Put: UInt16);
begin
  Bytes_Put := FOutStream.Write(Data, NumBytes);
end;

procedure TLZH.GetBytes(var Data; NumBytes: UInt16; var Bytes_Got: UInt16);
begin
  Bytes_Got := FInStream.Read(Data, NumBytes);
end;

procedure TLZH.Encode(var Bytes_Written: Int32);
var ct : BYTE;
    i, len2, r, s, last_match_length : Int16;
    Got : UInt16;
begin
  InitLZH;

  textsize := 0;   // rewind and rescan
  StartHuff;
  InitTree;
  s := 0;
  r := N - F;
  FillChar(Text_buf^[0],r,' ');
  len2 := 0;
  Got := 1;
  while (len2 < F) and (Got <> 0) do begin
    GetBytes(ct,1,Got);
    if Got <> 0 then begin
      text_buf^[r + len2] := ct;
      Inc(len2);
    end;
  end;

  textsize := len2;
  for i := 1 to F do InsertNode(r - i);
  InsertNode(r);

  repeat
    if (match_length > len2) then match_length := len2;
    if (match_length <= THRESHOLD) then begin
      match_length := 1;
      EncodeChar(text_buf^[r]);
    end
    else begin
      EncodeChar(255 - THRESHOLD + match_length);
      EncodePosition(match_position);
    end;
    last_match_length := match_length;
    i := 0;
    Got := 1;

    while (i < last_match_length) and (Got <> 0) do begin
      GetBytes(ct,1,Got);
      if Got <> 0 then begin
        DeleteNode(s);
        text_buf^[s] := ct;
        if (s < Pred(F)) then text_buf^[s + N] := ct;
        s := SUCC(s) and Pred(N);
        r := SUCC(r) and Pred(N);
        InsertNode(r);
        inc(i);
      end;
    end;

    Inc(textsize,i);

    while (i < last_match_length) do begin
      Inc(i);
      DeleteNode(s);
      s := SUCC(s) and Pred(N);
      r := SUCC(r) and Pred(N);
      DEC(len2);
      if BOOLEAN(len2) then InsertNode(r);
    end;

  until (len2 <= 0);

  EncodeEnd;
  EndLZH;
  Bytes_Written := TextSize;
end;

procedure TLZH.Decode(UnpackedSize: Int32);
var c, i, j, k, r : Int16;
    c2,a : Byte;
    count : Int32;
    Put : UInt16;
begin
  InitLZH;
  StartHuff;
  r := N - F;
  FillChar(text_buf^[0],r,' ');
  Count := 0;

  while count < UnpackedSize do begin
    c := DecodeChar;
    if (c < 256) then begin
      c2 := Lo(c);
      PutBytes(c2,1,Put);
      text_buf^[r] := c;
      Inc(r);
      r := r and Pred(N);
      Inc(count);
    end
    else begin
      i := (r - SUCC(DecodePosition)) and Pred(N);
      j := c - 255 + THRESHOLD;

      for k := 0 to Pred(j) do begin
        c := text_buf^[(i + k) and Pred(N)];
        c2 := Lo(c);
        PutBytes(c2,1,Put);
        text_buf^[r] := c;
        Inc(r);
        r := r and Pred(N);
        Inc(count);
      end;
   end;
  end;

  endLZH;
end;

constructor TLZH.Create(InStream, OutStream: TStream);
begin
  inherited Create;
  FInStream := InStream;
  FOutStream := OutStream;
end;


end.

