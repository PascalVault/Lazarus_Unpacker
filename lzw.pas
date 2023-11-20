unit lzw;

interface

//Based on unlzw.c (v 0.15 1993/06/10 13:28:35) from Gzip v.1.
//Ported to Delphi by S. Fabricius
//Additional changes: https://github.com/PascalVault
//License: MIT

uses classes,sysutils,windows;

const
  TBL_CLEAR = 256;
  TBL_FIRST = TBL_CLEAR + 1;
  LZW_MAGIC = $1f9d;
  BITS = 16;
  INIT_BITS = 9;
  BIT_MASK = $1f;
  HDR_EXTENDED = $20;
  HDR_FREE = $40;
  dBLOCK_MODE = $80;
  LZW_RESERVED = $60;
  INBUFSIZ = $8000;
  INBUF_EXTRA = 64;
  OUTBUFSIZ = $4000;
  OUTBUF_EXTRA = 2048;
  DIST_BUFSIZE = $8000;
  WSIZE = $8000;

type
  EUxCDecompession = class(Exception);
  EUxCMagic = class(Exception);

  { TLZW }

  TLZW = class
  private
    tab_prefix : array [0.. 1 shl BITS - 1] of word;
    tab_suffix : array [0.. 2 * WSIZE - 1]of Byte;
    outbuf : array [0.. INBUFSIZ + INBUF_EXTRA - 1] of Byte;
    inbuf : array [0.. OUTBUFSIZ + OUTBUF_EXTRA - 1] of Byte;
    de_stack : array[0.. DIST_BUFSIZE - 2] of byte;
    inptr : integer;
    maxbits : Byte;
    bytes_in : integer;
    bytes_out : integer;
    insize : integer;
    rsize : integer;
    block_mode : Boolean;

    inStr,outStr:TStream;

    procedure memcpy(var src: array of byte; srcStart: Integer;var dst: array of byte; dstStart: Integer; len: integer);
    procedure input(var b: array of Byte; var o: Integer; var c: Integer; n: Integer; m: Integer);
    function get_byte(str: TStream): Byte;
  public
    constructor Create(InF, OutF: TStream);
    function Decode(Bits: Byte; PackedSize: Int64): Boolean;
  end;


implementation


procedure TLZW.memcpy(var src: array of byte; srcStart: Integer;var dst: array of byte; dstStart: Integer; len: integer);
var
 i : integer;
 buf : array of byte;
begin
 setlength(buf,len);
 for i := 0 to len - 1 do
  buf[i]:=src[srcStart+i];
 for i := 0 to len - 1 do
  dst[dstStart+i]:=buf[i];
end;

procedure TLZW.input(var b: array of Byte; var o: Integer; var c: Integer; n: Integer; m: Integer);
var
 p: integer;
begin
 p := o shr 3;
 c := (((b[p] and $FF) or ((b[p + 1] and $FF) shl 8) or ((b[p + 2] and $FF) shl 16)) shr (o and $7)) and m;
 o := o + n;
end;

function TLZW.get_byte(str: TStream): Byte;
begin
 str.Read(Result,1);
end;

constructor TLZW.Create(InF, OutF: TStream);
begin
  block_mode := false;
  inStr := InF;
  outStr := OutF;
end;

function TLZW.Decode(Bits: Byte; PackedSize: Int64): Boolean;
var
 stackp : Integer;
 code : LongInt;
 finchar : byte;
 oldcode : LongInt;
 incode : LongInt;
 inbits : LongInt;
 posbits : LongInt;
 outpos : integer;
 bitmask : Word;
 free_ent : LongInt;
 maxcode : LongInt;
 maxmaxcode : LongInt;
 n_bits : Integer;
 magic : Word;
 i, e, o : integer;
 label ResetBuf;
begin
 {
 magic := get_byte(inStr) shl 8;
 magic := magic + get_byte(inStr);

 if magic <> LZW_MAGIC then
  raise EUxCMagic.Create('Input not in compress format (read magic number 0x'+IntToHex(magic,4)+')');

 maxbits:=get_byte(inStr);
 }
 maxbits := bits;

 block_mode := (maxbits and dBLOCK_MODE) > 0;

 maxbits := maxbits and BIT_MASK;
 maxmaxcode := 1 shl maxbits;

 if maxbits > BITS then
  raise EUxCDecompession.Create('compressed with '+IntToStr(maxbits)+' bits, but can only handle '+IntToStr(BITS)+' bits');

 inptr := 0;
 insize := 0;
 rsize := -1;
 n_bits := INIT_BITS;
 maxcode := (1 shl n_bits) - 1;
 bitmask := maxcode;
 oldcode := -1;
 finchar := 0;
 outpos := 0;
 bytes_in := 3;
 bytes_out := 0;
 posbits := inptr shl 3;
 if block_mode then
  free_ent := TBL_FIRST
 else
  free_ent := 256;

 FillChar(tab_prefix,256,$0);

 for code:=255 downto 0 do
  tab_suffix[code]:=code;

 while rsize <> 0 do
  begin

   ResetBuf :
   o := posbits shr 3;
   e := insize - o;

	 for i := 0 to e - 1 do
    inbuf[i] := inbuf[i+o];

   insize := e;
   posbits := 0;

   if insize < INBUF_EXTRA then
    begin
     rsize:=inStr.Read(inbuf[insize],INBUFSIZ);
     insize := insize + rsize;
     bytes_in := bytes_in + rsize;
    end;

   if rsize > 0 then
    inbits := (insize - insize mod n_bits) shl 3
   else
    inbits := (insize shl 3) - (n_bits - 1);

   while inbits > posbits do
    begin

     if free_ent > maxcode then
      begin
       posbits := (posbits - 1) + ((n_bits shl 3) - (posbits - 1 + (n_bits shl 3)) mod (n_bits shl 3));
       inc(n_bits);
       if n_bits = maxbits then
        maxcode := maxmaxcode
       else
        maxcode := (1 shl n_bits) - 1;
       bitmask := (1 shl n_bits) - 1;
       goto ResetBuf;
      end;

     input(inbuf,posbits,code,n_bits,bitmask);

     if oldcode = -1 then
      begin
       if code >= 256 then
        raise EUxCDecompession.Create('corrupt input: code=' + intToStr(code) + ' > 255');
       finchar := code;
       oldcode := code;
       outbuf[outpos] := finchar;
       inc(outpos);
       continue;
      end;

     // handle CLEAR code

     if (code = TBL_CLEAR) and block_mode then
      begin
       FillChar(tab_prefix,256,$0);
       free_ent := TBL_FIRST - 1;
       posbits := (posbits - 1) + ((n_bits shl 3) - (posbits - 1 + (n_bits shl 3)) mod (n_bits shl 3));
       n_bits := INIT_BITS;
       maxcode := (1 shl n_bits) - 1;
       bitmask := maxcode;
       goto ResetBuf;
      end;

     // setup

     incode := code;
     stackp := Length(de_stack);

     // Handle KwK case

     if code >= free_ent then
      begin
       if code > free_ent then
        begin
         if outpos > 0 then
          begin
           outStr.write(outbuf[0],outpos);
           bytes_out := bytes_out + outpos;
          end;
         raise EUxCDecompession.Create('corrupt input: code=' + intToStr(code) + ', free_ent=' + intToStr(free_ent) + ', Bytes in=' + intToStr(bytes_in) + ', Bytes out=' + intToStr(bytes_out));
        end;
       dec(stackp);
       de_stack[stackp] := finchar;
       code := oldcode;
      end;

     // Generate output characters in reverse order

     while code >= 256 do
      begin
       dec(stackp);
       de_stack[stackp] := tab_suffix[code];
       code := tab_prefix[code];
      end;
     finchar := tab_suffix[code];
     dec(stackp);
     de_stack[stackp] := finchar;

     // And put them out in forward order

     i := length(de_stack) - stackp;
     if outpos + i >= OUTBUFSIZ then
      begin
       while i <> -1 do
        begin
         if i > OUTBUFSIZ - outpos then i := OUTBUFSIZ - outpos;
         if i > 0 then
          begin
           memcpy(de_stack,stackp,outbuf,outpos,i);
           outpos := outpos + i;
          end;
         if outpos >= OUTBUFSIZ then
          begin
           outStr.write(outbuf[0],outpos);
           bytes_out := bytes_out + outpos;
           outpos := 0;
          end;
         stackp := stackp + i;
         i := length(de_stack) - stackp;
         if i = 0 then i:=-1;
        end;
      end
     else
      begin
       memcpy(de_stack,stackp,outbuf,outpos,i);
       outpos := outpos + i;
      end;

     // generate new entry in table

     code := free_ent;
     if free_ent < maxmaxcode then
      begin
       tab_prefix[code] := oldcode;
       tab_suffix[code] := finchar;
       free_ent := code + 1;
      end;

     // Remember previous code

     oldcode := incode;
    end;
  end;

 if outpos > 0 then
  begin
   outStr.write(outbuf[0],outpos);
   bytes_out := bytes_out + outpos;
  end;
 Result := true;
end;


end.
