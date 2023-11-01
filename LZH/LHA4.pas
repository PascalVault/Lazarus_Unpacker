unit LHA4;

interface
{$mode objfpc}
{$R+}
uses Classes, SysUtils, Dialogs, Math;

// LZSS compression and adaptive Huffman coding
// used in .LZH archives as "LH4"-"LH7" compression methods
// Based on AR code by Haruhiko OKUMURA
// Translated to Pascal and modified: github.com/PascalVault
//
// Thanks to alpine and TRon for finding bugs

type
 TLHA_Base = class
  private const
    CHAR_BIT = 8;
    UCHAR_MAX = 255;
    BITBUFSIZ = CHAR_BIT * 4;
    //DICBIT = 13;    // 12(-lh4-) or 13(-lh5-)
    MAXMATCH = 256; // formerly F (not more than UCHAR_MAX + 1)
    THRESHOLD = 3;  // choose optimal value
    //NC = UCHAR_MAX + MAXMATCH + 2 - THRESHOLD; // alphabet = {0, 1, 2, ..., NC - 1}
    NC = 510;
    CBIT = 9;  // $\lfloor \log_2 NC \rfloor + 1$
    CODE_BIT = 16;  // codeword length
    NT = CODE_BIT + 3;
    //PBIT = 4;
    TBIT = 5;

    //MAX values for LH4-LHX
    NP_TOP = 21;
    NPT_TOP = 21;
    DICSIZ_TOP = 1 shl 20;
  private
    c_table: array[0..4095] of Word;
    pt_table: array[0..255] of Word;
    left: array[0..2*NC-1] of Word;
    right: array[0..2*NC - 1] of Word;
    c_len: array[0..NC - 1] of Byte;
    c_freq: array[0..2*NC - 1] of Word;
    c_code: array[0..NC - 1] of Word;
    t_freq: array[0..2*NT - 1] of Word;
    //
    p_freq: array[0..2*NP_TOP - 1] of Word;
    //
    pt_code: array[0..NPT_TOP-1] of Word;
    pt_len: array[0..NPT_TOP-1] of Byte;
    buffer: array[0..DICSIZ_TOP-1] of Byte;
  private
    DICBIT: Word;
    PBIT: Word;
    DICSIZ: Cardinal;
    NP: Word;
    NPT: Word;
  private
    bitbuf: Cardinal;
    subbitbuf: Cardinal;
    bitcount: LongInt;
    blocksize: LongInt;
    jj: LongInt;
    compsize: LongInt;
    iii: LongInt;
    procedure fillbuf(n: LongInt);
    function getbits(n: LongInt): LongInt;
    procedure fwrite_crc(n: LongInt);
    procedure init_getbits;
    procedure make_table(nchar: LongInt; var bitlen: array of Byte; tablebits: LongInt; var table: array of Word);
    procedure read_pt_len(nn: LongInt; nbit: LongInt; i_special: LongInt);
    procedure read_c_len;
    function decode_c: LongInt;
    function decode_p: LongInt;
    procedure huf_decode_start;
    procedure decode_start;
    procedure decode_buf(count: LongInt);
  private
    FInStream: TStream;
    FOutStream: TStream;
  public
    constructor Create(InStream, OutStream: TStream); overload;
    procedure Decode(UnpackedSize: Cardinal);
  end;

  { TLHA4 }

  TLHA4 = class(TLHA_Base)
  public
    constructor Create(InStream, OutStream: TStream); overload;
  end;

  { TLHA5 }

  TLHA5 = class(TLHA_Base)
  public
    constructor Create(InStream, OutStream: TStream); overload;
  end;

  { TLHA6 }

  TLHA6 = class(TLHA_Base)
  public
    constructor Create(InStream, OutStream: TStream); overload;
  end;

  { TLHA7 }

  TLHA7 = class(TLHA_Base)
  public
    constructor Create(InStream, OutStream: TStream); overload;
  end;

  { TLHAX }

  TLHAX = class(TLHA_Base)
  public
    constructor Create(InStream, OutStream: TStream); overload;
  end;

implementation

{ TLHA4 }

constructor TLHA4.Create(InStream, OutStream: TStream);
begin
  DICBIT := 13; //works with 12 too
  PBIT := 4;

  inherited Create(InStream, OutStream);
end;

{ TLHA5 }

constructor TLHA5.Create(InStream, OutStream: TStream);
begin
  DICBIT := 14;
  PBIT := 4;

  inherited Create(InStream, OutStream);
end;

{ TLHA6 }

constructor TLHA6.Create(InStream, OutStream: TStream);
begin
  DICBIT := 16;
  PBIT := 5;

  inherited Create(InStream, OutStream);
end;

{ TLHA7 }

constructor TLHA7.Create(InStream, OutStream: TStream);
begin
  DICBIT := 17;
  PBIT := 5;

  inherited Create(InStream, OutStream);
end;

{ TLHAX }

constructor TLHAX.Create(InStream, OutStream: TStream);
begin
  DICBIT := 20;
  PBIT := 5;

  inherited Create(InStream, OutStream);
end;

constructor TLHA_base.Create(InStream, OutStream: TStream);
begin
  DICSIZ := 1 shl DICBIT;
  NP := DICBIT + 1;
  if NT > NP then NPT := NT
  else            NPT := NP;

  FInStream := InStream;
  FOutStream := OutStream;
end;

procedure TLHA_Base.Decode(UnpackedSize: Cardinal);
var n: LongInt;
begin
  iii := 0;

  compsize := FInStream.Size;

  decode_start();
  while UnpackedSize <> 0 do
  begin
      n := Min(UnpackedSize, DICSIZ);
      decode_buf(n);

      fwrite_crc(n);
      UnpackedSize := UnpackedSize - n;
  end;
end;

procedure TLHA_Base.fillbuf(n: LongInt);
var Ch: Byte;
begin
  bitbuf := (bitbuf shl n);

  while n > bitcount do
  begin
    n := n - bitcount;
    bitbuf := bitbuf or (subbitbuf shl n );

    if compsize <> 0 then
    begin
      Dec(compsize);
      FInStream.read(ch, 1);
      subbitbuf := Ch;
    end
    else
      subbitbuf := 0;
    bitcount := CHAR_BIT;
  end;
  bitcount := bitcount - n;
  bitbuf := bitbuf or subbitbuf shr bitcount;
end;

function TLHA_Base.getbits(n: LongInt): LongInt;
var x: LongInt;
    Ch: Byte;
begin
  if n = 0 then
  begin
    Result := 0;
    Exit;
  end;
  x := bitbuf shr (BITBUFSIZ - n);
  bitbuf := (bitbuf shl n);

  while n > bitcount do
  begin
    n := n - bitcount;
    bitbuf := bitbuf or (subbitbuf shl n);

    if compsize <> 0 then
    begin
      Dec(compsize);
      FInStream.read(Ch, 1);
      subbitbuf := Ch;
    end
    else
      subbitbuf := 0;
    bitcount := CHAR_BIT;
  end;
  bitcount := bitcount - n;

  bitbuf := bitbuf or (subbitbuf shr bitcount);
  Result := x;
end;

procedure TLHA_Base.fwrite_crc(n: LongInt);
begin
  if FOutStream.write(buffer[0], n) < 1 then raise Exception.Create('Unable to write');
end;

procedure TLHA_Base.init_getbits;
begin
  bitbuf := 0;
  subbitbuf := 0;
  bitcount := 0;
  fillbuf(BITBUFSIZ);
end;

procedure TLHA_Base.make_table(nchar: LongInt; var bitlen: array of Byte; tablebits: LongInt; var table: array of Word);
var count: array[0..16] of Word;
    weight: array[0..16] of Word;
    start: array[0..17] of Word;
    jutbits, avail, mask, ch: Cardinal;
    len, nextcode, k, l, i: Cardinal;
    p: PWordArray;
begin

  for i := 0 to 16 do count[i] := 0;
  for i := 0 to nchar - 1 do begin
    count[bitlen[i]] := count[bitlen[i]] + 1;
  end;

  start[1] := 0;
  for i := 1 to 16 do
    start[i + 1] := (start[i] + (count[i] shl (16 - i))) and $FFFF;

  if start[17] <> 0 then begin
    raise Exception.Create('Bad table');
    Exit;
  end;

  jutbits := 16 - tablebits;
  for i := 1 to tablebits do
  begin
    start[i] := start[i] shr jutbits;
    weight[i] := 1 shl (tablebits - i);
  end;

  i := tablebits+1;
  while i <= 16 do
  begin
    weight[i] := 1 shl (16 - i);
    Inc(i);
  end;
  i := start[tablebits + 1] shr jutbits;
  if i <> 0 then
  begin
    k := 1 shl tablebits;
    while i <> k do
    begin
      table[i] := 0;
      Inc(i);
    end;
  end;

  avail := nchar;
  mask := 1 shl (15 - tablebits);
  for ch := 0 to nchar - 1 do
  begin
    len := bitlen[ch];
    if len = 0 then continue;
    nextcode := start[len] + weight[len];
    if len <= tablebits then
    begin
      for i := start[len] to nextcode - 1 do table[i] := ch;
    end
    else
    begin
      k := start[len];
      p := @table;
      l := k shr jutbits;
      i := len - tablebits;
      while i <> 0 do
      begin
        if p^[l] = 0 then
        begin
          right[avail] := 0;
          left[avail] := 0;
          p^[l] := avail;
          Inc(avail);
        end;
        if k and mask <> 0 then
        begin
          l := p^[l];
          p := @right;
        end
        else
        begin
          l := p^[l];
          p := @left;
        end;
        k := k shl 1;
        Dec(i);
      end;
      p^[l] := ch;
    end;
    start[len] := Word(nextcode);
  end;
end;

procedure TLHA_Base.read_pt_len(nn: LongInt; nbit: LongInt; i_special: LongInt);
var i: LongInt;
    c: LongInt;
    n: LongInt;
    mask: LongInt;
begin
  i := 0;
  c := 0;
  n := 0;
  mask := 0;

  n := getbits(nbit);
  if n = 0 then
  begin
    c := getbits(nbit);
    for i := 0 to nn - 1 do pt_len[i] := 0;
    for i := 0 to 255 do    pt_table[i] := c;
  end
  else
  begin
    i := 0;
    while i < n do
    begin
      c := bitbuf shr (BITBUFSIZ - 3);
      if c = 7 then
      begin
        mask := 1 shl (BITBUFSIZ - 1 - 3);
        while mask and bitbuf <> 0 do
        begin
          mask := mask shr 1;
          Inc(c);
        end;
      end;
      fillbuf(IfThen(c < 7, 3, c - 3));
      pt_len[i] := c;
      Inc(i);

      if i = i_special then
      begin
        c := getbits(2);
        Dec(c);
        while c >= 0 do
        begin
          pt_len[i] := 0;
          Inc(i);
          Dec(c);
        end;
      end;
    end;
    while i < nn do
    begin
      pt_len[i] := 0;
      Inc(i);
    end;
    make_table(nn, pt_len, 8,  pt_table);
  end;
end;

procedure TLHA_Base.read_c_len;
var i: LongInt;
    c: LongInt;
    n: LongInt;
    mask: LongInt;
begin
  i := 0;
  c := 0;
  n := 0;
  mask := 0;

  n := getbits(CBIT);
  if n = 0 then
  begin
    c := getbits(CBIT);
    for i := 0 to NC - 1 do c_len[i] := 0;
    for i := 0 to 4095 do   c_table[i] := c;
  end
  else
  begin
    i := 0;
    while i < n do
    begin
      c := pt_table[bitbuf shr (BITBUFSIZ - 8)];
      if c >= NT then
      begin
        mask := 1 shl (BITBUFSIZ - 1 - 8);
        repeat
          if bitbuf and mask <> 0 then c := right[c]
          else                         c := left[c];
          mask := mask shr 1;
        until c < NT;
      end;
      fillbuf(pt_len[c]);
      if c <= 2 then
      begin
        if c = 0      then c := 1
        else if c = 1 then c := getbits(4) + 3
        else               c := getbits(CBIT) + 20;
        Dec(c);

        while c >= 0 do
        begin
          c_len[i] := 0;
          Inc(i);
          Dec(c);
        end;
      end
      else begin
        c_len[i] := c - 2;
        Inc(i);
      end;
    end;
    while i < NC do
    begin
      c_len[i] := 0;
      Inc(i);
    end;
    make_table(NC, c_len, 12,  c_table);
  end;
end;

function TLHA_Base.decode_c: LongInt;
var j: LongInt;
    mask: LongInt;
    ch: Byte;
    n: LongInt;
begin
  j := 0;
  mask := 0;

  if blocksize = 0 then
  begin
    blocksize := getbits(16);
    read_pt_len(NT, TBIT, 3);
    read_c_len();
    read_pt_len(NP, PBIT, -1);
  end;
  Dec(blocksize);
  j := c_table[bitbuf shr (BITBUFSIZ - 12)];
  if j >= NC then
  begin
    mask := 1 shl (BITBUFSIZ - 1 - 12);
    repeat
      if bitbuf and mask <> 0 then j := right[j]
      else                         j := left[j];
      mask := mask shr 1;
    until j < NC;
  end;
  // inline fillbuf
  n := c_len[j];
  bitbuf := (bitbuf shl n);

  while (n > bitcount) do begin
     n := n - bitcount;
     bitbuf := bitbuf or (subbitbuf shl n);

      if (compsize <> 0) then begin
          dec(compsize);
          FInStream.read(ch, 1);
          subbitbuf := ch;
      end
      else subbitbuf := 0;
      bitcount := CHAR_BIT;
  end;

  bitcount := bitcount - n;
  bitbuf := bitbuf or (subbitbuf shr bitcount);

  result := j;
end;

function TLHA_Base.decode_p: LongInt;
var j: LongInt;
    mask: LongInt;
    ch: Byte;
    n: LongInt;
begin
  j := pt_table[bitbuf shr (BITBUFSIZ - 8)];
  if j >= NP then
  begin
    mask := 1 shl (BITBUFSIZ - 1 - 8);
    repeat
      if bitbuf and mask <> 0 then j := right[j]
      else                         j := left[j];
      mask := mask shr 1;
    until j < NP;
  end;
  // inline fillbuf
  n := pt_len[j];
  bitbuf := (bitbuf shl n);

  while (n > bitcount) do begin
      n := n - bitcount;
      bitbuf := bitbuf or (subbitbuf shl n);

      if (compsize <> 0) then begin
          dec(compsize);
          FInStream.read(ch, 1);
          subbitbuf := ch;
      end
      else subbitbuf := 0;
      bitcount := CHAR_BIT;
  end;

  bitcount := bitcount - n;
  bitbuf := bitbuf or (subbitbuf shr bitcount);

  if (j <> 0) then j := (1 shl (j - 1)) + getbits(j - 1);
  Result := j;
end;

procedure TLHA_Base.huf_decode_start;
begin
  init_getbits();
  blocksize := 0;
end;

procedure TLHA_Base.decode_start;
begin
  huf_decode_start();
  jj := 0;
end;

procedure TLHA_Base.decode_buf(count: LongInt);
var r: LongInt;
    c: LongInt;
begin
  r := 0;
  c := 0;

  Dec(jj);
  while jj >= 0 do
  begin
    buffer[r] := buffer[iii];
    iii := (iii + 1) and (DICSIZ - 1);
    Inc(r);
    if r = count then Exit;
    Dec(jj);
  end;
  while True do
  begin
    c := decode_c();

    if c <= UCHAR_MAX then
    begin
      buffer[r] := c;
      Inc(r);
      if r = count then Exit;
    end
    else
    begin
      jj := c - (UCHAR_MAX + 1 - THRESHOLD);
      iii := (r - decode_p() - 1) and (DICSIZ - 1);

      Dec(jj);
      while jj >= 0 do
      begin
        buffer[r] := buffer[iii];
        iii := (iii + 1) and (DICSIZ - 1);
        Inc(r);
        if r = count then Exit;
        Dec(jj);
      end;
    end;
  end;
end;

end.
