unit ArcMethod;

interface
{$mode objfpc}{$H+}

//Based on DEARC.PAS from 1988
//Authors: Roy Collins, David W. Carroll, Richard P. Byrne, Paul Roub, PascalVault
//https://github.com/PascalVault
//Licence: MIT

uses
  classes, sysutils, dialogs;

const
  BLOCKSIZE  = 512;             // I/O block size
  strlen     = 100;             // standard string length
  fnlen      = 12;              // file name length - 1
  ERROR   = -1;
  SPEOF   = 256;
  NUMVALS = 256;                // 1 less than the number of values
  DLE = $90;
  Crunch_BITS = 12;
  Squash_BITS = 13;
  INIT_BITS = 9;
  FIRST = 257;
  CLEAR = 256;
  HSIZEM1 = 8191;
  BITSM1 = 12;
  RMASK : array[0..8] of byte = ($00, $01, $03, $07, $0f, $1f, $3f, $7f, $ff);
  TABSIZE   = 4096;
  TABSIZEM1 = 4095;
  NO_PRED : word  = $FFFF;
  EMPTY   : word  = $FFFF;
  MaxInt16 = 32767;

type
  strtype = string[strlen];
  fntype  = array [0..fnlen] of char;
  buftype = array [1..BLOCKSIZE] of byte;
  nd = record
          child : array [0..1] of Int16
        end;
  entry = packed record
             used         : boolean;
             next         : Int16;
             predecessor  : Int16;
             follower     : byte
          end;

type
  { TArcMethod }

  TArcMethod = class
  protected
    arcfile  : TStream;
    arcbuf   : buftype;
    arcptr   : Int16;
    endfile  : boolean;
    extfile  : TStream;
    extbuf   : buftype;
    extptr   : Int16;
    size     : longint;
    state    : (NOHIST, INREP);
    firstch  : boolean;
    lastc    : Int16;
    code       : Int16;
    oldcode    : Int16;
    finchar    : Int16;
    bits,
    n_bits,
    maxcode    : Int16;
    prefix     : array[0..HSIZEM1] of Int16;
    suffix     : array[0..HSIZEM1] of byte;
    buf        : array[0..BITSM1]  of byte;
    clear_flg  : Int16;
    stack1     : array[0..HSIZEM1] of byte;
    free_ent   : Int16;
    maxcodemax : Int16;
    offset,
    sizex      : Int16;
    procedure Read_Block;
    procedure Write_Block;
    function get_arc : byte;
    procedure put_ext(c : byte);
    procedure putc_unp(c : Int16);
    procedure putc_ncr(c : Int16);
    function getc_unp : Int16;
    procedure open_arc;
    procedure open_ext;
    procedure close_arc;
    procedure close_ext;
    function getcode : Int16;
    procedure decomp(SquashFlag : Int16);
  public
    constructor Create(InF, OutF: TStream);
  end;

  { TCrunch8 }

  TCrunch8 = class(TArcMethod)
  public
    procedure Decode(PackedSize: Int64);
  end;

  TSquash = class(TArcMethod)
  public
    procedure Decode(PackedSize: Int64);
  end;

implementation

// read a block from the archive file
procedure TArcMethod.Read_Block;
var res : Int64;
begin
  res := arcfile.read(arcbuf, BLOCKSIZE);
  if res < 0 then endfile := True;

  arcptr := 1;
end;

procedure TArcMethod.Write_Block;
begin
  extfile.Write(extbuf, extptr);
  extptr := 1
end;

// read 1 character from the archive file
function TArcMethod.get_arc : byte;
begin
  if endfile then
    get_arc := 0
  else
    begin
      get_arc := arcbuf[arcptr];
      if arcptr = BLOCKSIZE then
        Read_Block
      else
        arcptr := arcptr + 1
    end
end;

//write 1 character to the extracted file
procedure TArcMethod.put_ext(c : byte);
begin
  extbuf[extptr] := c;
  if extptr = BLOCKSIZE then
    Write_Block
  else
    extptr := extptr + 1
end;

// put one character to extracted file
procedure TArcMethod.putc_unp(c : Int16);
begin
  put_ext(c)
end;

// put one char,  checking for run-length compression
procedure TArcMethod.putc_ncr(c : Int16);
begin
  case state of
    NOHIST :
      if c = DLE then
        state := INREP
      else
        begin
          lastc := c;
          putc_unp(c)
        end;

    INREP  :
      begin
        if c = 0 then
          putc_unp(DLE)
        else
          begin
            c := c - 1;
            while (c <> 0) do
              begin
                putc_unp(lastc);
                c := c - 1
              end
            end;

        state := NOHIST
      end
  end  // case
end;


//get one character from archive
function TArcMethod.getc_unp : Int16;
begin
  if size = 0.0 then
    getc_unp := -1
  else
    begin
      size := size - 1;
      getc_unp := get_arc
    end;
end;

//open the archive file for input processing
procedure TArcMethod.open_arc;
begin
  endfile := FALSE;
  Read_Block
end;

//open the extracted file for writing
procedure TArcMethod.open_ext;
begin
  extptr := 1;
end;


// close the archive file
procedure TArcMethod.close_arc;
begin
  //arcfile.free;
end;

// close the extracted file
procedure TArcMethod.close_ext;
var
  dt     : longint;
  handle : word;
begin
  extptr := extptr - 1;

  if (extptr <> 0) then
    Write_Block;

  //extfile.free;
end;

function TArcMethod.getcode : Int16;
var
  r_off, bitsx : Int16;
  bp : byte;
  skipped: Boolean;
  sizex2: Int16;
begin
  if firstch then
    begin
      offset := 0;
      sizex := 0;
      firstch := false;
    end;

  bp := 0;

  if (clear_flg > 0) or (offset >= sizex) or (free_ent > maxcode) then
    begin
      if free_ent > maxcode then
        begin
          n_bits := n_bits + 1;
          if n_bits = BITS then
            maxcode := maxcodemax
          else
            maxcode := (1 shl n_bits) - 1;
        end;

      if clear_flg > 0 then
        begin
          n_bits := INIT_BITS;
          maxcode := (1 shl n_bits) - 1;
          clear_flg := 0;
        end;

      skipped := false;

      sizex2 := sizex;
      for sizex2 := 0 to n_bits-1 do
        begin
          code := getc_unp;
          if code = -1 then begin
            skipped := true;
            break;
          end
          else
            buf[sizex2] := code;
        end;
      sizex := sizex2; //maybe not needed

      if not skipped then
      sizex := sizex + 1;

      if sizex <= 0 then
        begin
          result := -1;
          exit;
        end;

      offset := 0;
      sizex := (sizex shl 3) - (n_bits - 1);
    end;

  r_off := offset;
  bitsx := n_bits;

  //get first byte
  bp := bp + (r_off shr 3);
  r_off := r_off and 7;

  //get first part (low order bits)
  code := buf[bp] shr r_off;
  bp := bp + 1;
  bitsx := bitsx - (8 - r_off);
  r_off := 8 - r_off;

  if bitsx >= 8 then
    begin
      code := code or (buf[bp] shl r_off);
      bp := bp + 1;
      r_off := r_off + 8;
      bitsx := bitsx - 8;
    end;

  code := code or Int16(((buf[bp] and rmask[bitsx]) shl r_off));
  offset := offset + n_bits;
  result := code;
end;


// decompress a file with LZW
procedure TArcMethod.decomp(SquashFlag : Int16);
var
  stackp : Int16;
 // acode,
  incode : Int16;
  codee: Int16;
begin
  if SquashFlag = 0 then
    Bits := crunch_BITS
  else
    Bits := squash_BITS;

  if firstch then
    maxcodemax := 1 shl bits;

  if SquashFlag = 0 then
    begin
      code := getc_unp;
      if code <> BITS then
        begin
           raise Exception.Create(Format('File packed with %d bits, I can only handle %d', [code, Bits] ));
           Exit;
        end;
    end;

  clear_flg := 0;
  n_bits := INIT_BITS;
  maxcode := (1 shl n_bits ) - 1;

  for codee := 255 downto 0 do
    begin
      prefix[codee] := 0;
      suffix[codee] := codee;
    end;

  free_ent := FIRST;
  oldcode := getcode;
  finchar := oldcode;

  if oldcode = -1 then
    exit;

  if SquashFlag = 0 then
    putc_ncr(finchar)
  else
    putc_unp(finchar);

  stackp := 0;

  code := getcode;
  while (code  > -1) do
    begin
      if code = CLEAR then
        begin
          for codee := 255 downto 0 do
            prefix[codee] := 0;
          clear_flg := 1;
          free_ent := FIRST - 1;
          code := getcode;
        end;

      incode := code;
      if code >= free_ent then
        begin
          stack1[stackp] := finchar;
          stackp := stackp + 1;
          code := oldcode;
        end;

      while (code >= 256) do
        begin
          stack1[stackp] := suffix[code];
          stackp := stackp + 1;
          code := prefix[code];
        end;

      finchar := suffix[code];
      stack1[stackp] := finchar;
      stackp := stackp + 1;
      repeat
        stackp := stackp - 1;
        If SquashFlag = 0 then
          putc_ncr(stack1[stackp])
        else
          putc_unp(stack1[stackp]);
      until stackp <= 0;

      code := free_ent;

      if code < maxcodemax then
        begin
          prefix[code] := oldcode;
          suffix[code] := finchar;
          free_ent := code + 1;
        end;

      oldcode := incode;
      code := getcode;
    end;
end;

constructor TArcMethod.Create(InF, OutF: TStream);
begin
  arcfile := InF;
  extfile := OutF;
end;

{ TSquash }

procedure TSquash.Decode(PackedSize: Int64);
begin
  open_arc;
  open_ext;

  Size := PackedSize;
  state   := NOHIST;
  FirstCh :=  TRUE;

  decomp(1);

  close_ext();
  close_arc;
end;

{ TCrunch8 }

procedure TCrunch8.Decode(PackedSize: Int64);
begin
  open_arc;
  open_ext;


  Size := PackedSize;
  state   := NOHIST;
  FirstCh :=  TRUE;

  decomp(0);

  close_ext();
  close_arc;
end;

end.
