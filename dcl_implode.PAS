unit dcl_implode;
{$mode delphi}


//Based on implode.c from "Lawine library" by ax.minaduki@gmail.com (May 2010)
//PKWare DCL implode compression
//https://github.com/PascalVault/Lazarus_Unpacker
//License changed from GNU LGPL to MIT with consent from ax.minaduki.

interface

uses Classes;

const
  IMPLODE_BINARY  = 0;                      //* Binary compression */
  IMPLODE_ASCII   = 1;                      //* ASCII compression */

  IMPLODE_DICT_1K = 4;                      //* Dictionary size is 1KB */
  IMPLODE_DICT_2K = 5;                      //* Dictionary size is 2KB */
  IMPLODE_DICT_4K = 6;                      //* Dictionary size is 4KB */

//* Bit sequences used to represent literal bytes */
const s_ChCode: Array[0..255] of Word = (
        $0490, $0fe0, $07e0, $0be0, $03e0, $0de0, $05e0, $09e0,
        $01e0, $00b8, $0062, $0ee0, $06e0, $0022, $0ae0, $02e0,
        $0ce0, $04e0, $08e0, $00e0, $0f60, $0760, $0b60, $0360,
        $0d60, $0560, $1240, $0960, $0160, $0e60, $0660, $0a60,
        $000f, $0250, $0038, $0260, $0050, $0c60, $0390, $00d8,
        $0042, $0002, $0058, $01b0, $007c, $0029, $003c, $0098,
        $005c, $0009, $001c, $006c, $002c, $004c, $0018, $000c,
        $0074, $00e8, $0068, $0460, $0090, $0034, $00b0, $0710,
        $0860, $0031, $0054, $0011, $0021, $0017, $0014, $00a8,
        $0028, $0001, $0310, $0130, $003e, $0064, $001e, $002e,
        $0024, $0510, $000e, $0036, $0016, $0044, $0030, $00c8,
        $01d0, $00d0, $0110, $0048, $0610, $0150, $0060, $0088,
        $0fa0, $0007, $0026, $0006, $003a, $001b, $001a, $002a,
        $000a, $000b, $0210, $0004, $0013, $0032, $0003, $001d,
        $0012, $0190, $000d, $0015, $0005, $0019, $0008, $0078,
        $00f0, $0070, $0290, $0410, $0010, $07a0, $0ba0, $03a0,
        $0240, $1c40, $0c40, $1440, $0440, $1840, $0840, $1040,
        $0040, $1f80, $0f80, $1780, $0780, $1b80, $0b80, $1380,
        $0380, $1d80, $0d80, $1580, $0580, $1980, $0980, $1180,
        $0180, $1e80, $0e80, $1680, $0680, $1a80, $0a80, $1280,
        $0280, $1c80, $0c80, $1480, $0480, $1880, $0880, $1080,
        $0080, $1f00, $0f00, $1700, $0700, $1b00, $0b00, $1300,
        $0da0, $05a0, $09a0, $01a0, $0ea0, $06a0, $0aa0, $02a0,
        $0ca0, $04a0, $08a0, $00a0, $0f20, $0720, $0b20, $0320,
        $0d20, $0520, $0920, $0120, $0e20, $0620, $0a20, $0220,
        $0c20, $0420, $0820, $0020, $0fc0, $07c0, $0bc0, $03c0,
        $0dc0, $05c0, $09c0, $01c0, $0ec0, $06c0, $0ac0, $02c0,
        $0cc0, $04c0, $08c0, $00c0, $0f40, $0740, $0b40, $0340,
        $0300, $0d40, $1d00, $0d00, $1500, $0540, $0500, $1900,
        $0900, $0940, $1100, $0100, $1e00, $0e00, $0140, $1600,
        $0600, $1a00, $0e40, $0640, $0a40, $0a00, $1200, $0200,
        $1c00, $0c00, $1400, $0400, $1800, $0800, $1000, $0000);

//* Lengths of bit sequences used to represent literal bytes */
const s_ChBits: Array[0..255] of Byte = (
        $0b, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $08, $07, $0c, $0c, $07, $0c, $0c,
        $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0d, $0c, $0c, $0c, $0c, $0c,
        $04, $0a, $08, $0c, $0a, $0c, $0a, $08, $07, $07, $08, $09, $07, $06, $07, $08,
        $07, $06, $07, $07, $07, $07, $08, $07, $07, $08, $08, $0c, $0b, $07, $09, $0b,
        $0c, $06, $07, $06, $06, $05, $07, $08, $08, $06, $0b, $09, $06, $07, $06, $06,
        $07, $0b, $06, $06, $06, $07, $09, $08, $09, $09, $0b, $08, $0b, $09, $0c, $08,
        $0c, $05, $06, $06, $06, $05, $06, $06, $06, $05, $0b, $07, $05, $06, $05, $05,
        $06, $0a, $05, $05, $05, $05, $08, $07, $08, $08, $0a, $0b, $0b, $0c, $0c, $0c,
        $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d,
        $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d,
        $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d,
        $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c,
        $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c,
        $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c, $0c,
        $0d, $0c, $0d, $0d, $0d, $0c, $0d, $0d, $0d, $0c, $0d, $0d, $0d, $0d, $0c, $0d,
        $0d, $0d, $0c, $0c, $0c, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d);

//* Bit sequences used to represent the base values of the copy length */
const s_LenCode: Array[0..15] of Byte = (
        $05, $03, $01, $06, $0a, $02, $0c, $14, $04, $18, $08, $30, $10, $20, $40, $00);

//* Lengths of bit sequences used to represent the base values of the copy length */
const s_LenBits: Array[0..15] of Byte = (
        $03, $02, $03, $03, $04, $04, $04, $05, $05, $05, $05, $06, $06, $06, $07, $07);

//* Base values used for the copy length */
const s_LenBase: Array[0..15] of Word = (
        $0002, $0003, $0004, $0005, $0006, $0007, $0008, $0009,
        $000a, $000c, $0010, $0018, $0028, $0048, $0088, $0108);

//* Lengths of extra bits used to represent the copy length */
const s_ExLenBits: Array[0..15] of Byte = (
        $00, $00, $00, $00, $00, $00, $00, $00, $01, $02, $03, $04, $05, $06, $07, $08);

//* Bit sequences used to represent the most significant 6 bits of the copy offset */
const s_OffsCode: Array[0..63] of Byte = (
        $03, $0d, $05, $19, $09, $11, $01, $3e, $1e, $2e, $0e, $36, $16, $26, $06, $3a,
        $1a, $2a, $0a, $32, $12, $22, $42, $02, $7c, $3c, $5c, $1c, $6c, $2c, $4c, $0c,
        $74, $34, $54, $14, $64, $24, $44, $04, $78, $38, $58, $18, $68, $28, $48, $08,
        $f0, $70, $b0, $30, $d0, $50, $90, $10, $e0, $60, $a0, $20, $c0, $40, $80, $00);

//* Lengths of bit sequences used to represent the most significant 6 bits of the copy offset */
const s_OffsBits: Array[0..63] of Byte = (
        $02, $04, $04, $05, $05, $05, $05, $06, $06, $06, $06, $06, $06, $06, $06, $06,
        $06, $06, $06, $06, $06, $06, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07,
        $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07, $07,
        $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08);

type

  { TImplode }

  TDCL_Implode = class
  private
    FInStream, FOutStream: TStream;
    function TRUNCATE_VALUE(v: LongInt; b: Byte): LongInt;
    function pklib_implode(Ctype: Integer; dict: Integer; src: PByte; src_size: LongInt; dest: PByte; var dest_size: LongInt): Boolean;
    function pklib_explode(src: PByte; src_size: LongInt; dest: PByte; var dest_size: LongInt): Boolean;
  public
    constructor Create(InStream, OutStream: TStream);
    function Encode(Size: LongInt; out PackedSize: LongInt; CType: Integer; Dict: Integer): Boolean;
    function Decode(PackedSize, UnpackedSize: LongInt): Boolean;
  end;

implementation

function TDCL_Implode.TRUNCATE_VALUE(v: LongInt; b: Byte): LongInt;
begin
  result := ((v) AND ((1 SHL (b)) - 1));
end;

function TDCL_Implode.Encode(Size: LongInt; out PackedSize: LongInt; CType: Integer; Dict: Integer): Boolean;
var src: array of byte;
    src_size: integer;
    dest: array of byte;
    dest_size: integer;
begin
  src_size := Size;
  setLength(src, src_size);
  FInStream.Read(src[0], src_size);

  dest_size := src_size*10;
  setLength(dest, dest_size);

  Result := pklib_implode(1, 6, @src[0], src_size, @dest[0], dest_size);

  if Result then FOutStream.Write(dest[0], dest_size);
end;

function TDCL_Implode.Decode(PackedSize, UnpackedSize: LongInt): Boolean;
var src: array of byte;
    src_size: integer;
    dest: array of byte;
    dest_size: integer;
begin
  src_size := PackedSize;

  setLength(src, src_size);
  FInStream.Read(src[0], src_size);

  dest_size := UnpackedSize;
  setLength(dest, dest_size);

  Result := pklib_explode(@src[0], src_size, @dest[0], dest_size);

  if Result then FOutStream.Write(dest[0], dest_size);
end;

function TDCL_Implode.pklib_implode(Ctype: Integer; dict: Integer; src: PByte; src_size: LongInt; dest: PByte; var dest_size: LongInt): Boolean;
var
  i: Integer;                             // Index into tables
  ch: Byte;                               // Byte from input buffer
  max_copy_len: Integer;               // Length of longest duplicate data in the dictionary
  max_copy_ptr: PByte;    // Pointer to longest duplicate data in the dictionary
  copy_len: Integer;                   // Length of duplicate data in the dictionary
  copy_off: Integer;                   // Offset used in actual compressed data
  new_copy_off: Integer;               // Secondary offset used in actual compressed data
  copy_ptr: PByte;                // Pointer to duplicate data in the dictionary
  bak_copy_ptr: PByte;    // Temporarily holds previous value of copy_ptr
  new_rd_ptr: PByte;             // Secondary offset into input buffer
  new_dict_ptr: PByte;    // Secondary offset into dictionary
  rd_ptr: PByte;                 // Current position in input buffer
  wrt_ptr: PByte;                 // Current position in output buffer
  src_end_ptr: PByte;    // Pointer to the end of source buffer
  dest_end_ptr: PByte;    // Pointer to the end of dest buffer
  bit_num: Byte;                   // Number of bits in bit buffer
  bit_buf: LongInt;                  // Stores bits until there are enough to output a byte of data
  dict_ptr: PByte;                // Position in dictionary
  dict_size: LongInt;                 // Maximum size of dictionary
  cur_dict_size: LongInt;             // Current size of dictionary
  dict_buf: Array[0..4095] of Byte;    // Sliding dictionary used for compression and decompression
begin
  // Assume failure
  result := false;

  // Check for a valid compression type
  if ((Ctype <> IMPLODE_BINARY) AND (Ctype <> IMPLODE_ASCII)) then Exit;

  // Only dictionary sizes of 1024, 2048, and 4096 are allowed.
  // The values 4, 5, and 6 correspond with those sizes
  case (dict) of
    IMPLODE_DICT_1K:
      // Store actual dictionary size
      dict_size := 1024;
    IMPLODE_DICT_2K:
      // Store actual dictionary size
      dict_size := 2048;
    IMPLODE_DICT_4K:
      // Store actual dictionary size
      dict_size := 4096;
    else
      Exit;
  end;

  // Initialize buffer positions
  rd_ptr := src;
  wrt_ptr := dest;
  src_end_ptr := rd_ptr; Inc(src_end_ptr, src_size);
  dest_end_ptr := wrt_ptr; Inc(dest_end_ptr, dest_size);

  // Initialize dictionary position
  dict_ptr := @dict_buf;

  // Initialize current dictionary size to zero
  cur_dict_size := 0;

  // If the output buffer size is less than 4, there
  // is not enough room for the compressed data
  if ((dest_size < 4) AND NOT((src_size = 0) AND (dest_size = 4))) then Exit;

  // Store compression type and dictionary size
  wrt_ptr^ := Ctype; Inc(wrt_ptr);
  wrt_ptr^ := dict; Inc(wrt_ptr);

  // Initialize bit buffer
  bit_buf := 0;
  bit_num := 0;

  // Compress until input buffer is empty
  while (rd_ptr < src_end_ptr) do
  begin
      // Get a byte from the input buffer
      ch := rd_ptr^; Inc(rd_ptr);
      max_copy_len := 0;

      // If the dictionary is not empty, search for duplicate data in the dictionary
      if ((cur_dict_size > 1) AND (src_end_ptr - rd_ptr > 1)) then
      begin
          // Initialize offsets and lengths used in search
          copy_ptr := @dict_buf;
          max_copy_ptr := copy_ptr;
          max_copy_len := 0;

          // Store position of last written dictionary byte
          new_dict_ptr := dict_ptr; Dec(new_dict_ptr);
          if (new_dict_ptr < @dict_buf) then
          begin
              new_dict_ptr := @dict_buf; Inc(new_dict_ptr, cur_dict_size - 1);
          end;

          // Search dictionary for duplicate data
          while (copy_ptr < @dict_buf + cur_dict_size) do
          begin
              // Check for a match with first byte
              if (ch <> copy_ptr^) then
              begin
                Inc(copy_ptr);
                continue;
              end;

              bak_copy_ptr := copy_ptr;
              copy_len := 0;
              new_rd_ptr := rd_ptr; Dec(new_rd_ptr);

              // If there was a match, check for additional duplicate bytes
              repeat
                  // Increment pointers and length
                  Inc(copy_len);
                  Inc(new_rd_ptr);
                  Inc(copy_ptr);

                  // Wrap around pointer to beginning of dictionary buffer if the end of the buffer was reached
                  if (copy_ptr >= @dict_buf + dict_size) then
                    copy_ptr := @dict_buf;

                  // Wrap dictionary bytes if end of the dictionary was reached
                  if (copy_ptr = dict_ptr) then
                    copy_ptr := bak_copy_ptr;

                  // Stop checking for additional bytes if there is no more input or maximum length was reached
                  if ((copy_len >= 518) OR (new_rd_ptr >= src_end_ptr)) then
                    break;
              until (new_rd_ptr^ <> copy_ptr^);

              // Return the pointer to the beginning of the matching data
              copy_ptr := bak_copy_ptr;

              // Copying less than two bytes from dictionary wastes space, so don't do it ;)
              if ((copy_len < 2) OR (copy_len < max_copy_len)) then
              begin
                Inc(copy_ptr);
                continue;
              end;

              // Store the offset that will be outputted into the compressed data
              new_copy_off := (new_dict_ptr - (copy_ptr - cur_dict_size)) mod cur_dict_size;

              // If the length is equal, check for a more efficient offset
              if (copy_len = max_copy_len) then
              begin
                  // Use the most efficient offset
                  if (new_copy_off < copy_off) then
                  begin
                      copy_off := new_copy_off;
                      max_copy_ptr := copy_ptr;
                      max_copy_len := copy_len;
                  end;
              end
              // Only use the most efficient length and offset in dictionary
              else
              begin
                  // Store the offset that will be outputted into the compressed data
                  copy_off := new_copy_off;

                  // If the copy length is 2, check for a valid dictionary offset
                  if ((copy_len > 2) OR (copy_off <= 255)) then
                  begin
                      max_copy_ptr := copy_ptr;
                      max_copy_len := copy_len;
                  end;
              end;
              Inc(copy_ptr);
          end;

          // If there were at least 2 matching bytes in the dictionary that were found, output the length/offset pair
          if (max_copy_len >= 2) then
          begin
              // Reset the input pointers to the bytes that will be added to the dictionary
              Dec(rd_ptr);
              new_rd_ptr := rd_ptr; Inc(new_rd_ptr, max_copy_len);

              while (rd_ptr < new_rd_ptr) do
              begin
                  // Add a byte to the dictionary
                  dict_ptr^ := ch; Inc(dict_ptr);

                  // If the dictionary is not full yet, increment the current dictionary size
                  if (cur_dict_size < dict_size) then
                    Inc(cur_dict_size);

                  // If the current end of the dictionary is past the end of the buffer,
                  // wrap around back to the start
                  if (dict_ptr >= @dict_buf + dict_size) then
                    dict_ptr := @dict_buf;

                  // Get the next byte to be added
                  Inc(rd_ptr);
                  if (rd_ptr < new_rd_ptr) then
                    ch := rd_ptr^;
              end;

              // Find bit code for the base value of the length from the table
              for i := 0 to $0E do
              begin
                  if ((s_LenBase[i] <= max_copy_len) AND (max_copy_len < s_LenBase[i + 1])) then
                      break;
              end;

              // Store the base value of the length
              bit_buf := bit_buf + ((1 + (s_LenCode[i] shl 1)) shl bit_num);
              bit_num := bit_num + (1 + s_LenBits[i]);

              // Store the extra bits for the length
              bit_buf := bit_buf + ((max_copy_len - s_LenBase[i]) shl bit_num);
              bit_num := bit_num + (s_ExLenBits[i]);

              // Output the data from the bit buffer
              while (bit_num >= 8) do
              begin
                  // If output buffer has become full, stop immediately!
                  if (wrt_ptr >= dest_end_ptr) then Exit;

                  wrt_ptr^ := Byte(bit_buf); Inc(wrt_ptr);
                  bit_buf := bit_buf shr 8;
                  bit_num := bit_num - 8;
              end;

              // The most significant 6 bits of the dictionary offset are encoded with a
              // bit sequence then the first 2 after that if the copy length is 2,
              // otherwise it is the first 4, 5, or 6 (based on the dictionary size)
              if (max_copy_len = 2) then
              begin
                  // Store most significant 6 bits of offset using bit sequence
                  bit_buf := bit_buf + (s_OffsCode[copy_off shr 2] shl bit_num);
                  bit_num := bit_num + s_OffsBits[copy_off shr 2];

                  // Store the first 2 bits
                  bit_buf := bit_buf + ((copy_off AND $03) shl bit_num);
                  bit_num := bit_num + 2;
              end
              else
              begin
                  // Store most significant 6 bits of offset using bit sequence
                  bit_buf := bit_buf + (s_OffsCode[copy_off shr dict] shl bit_num);
                  bit_num := bit_num + s_OffsBits[copy_off shr dict];

                  // Store the first 4, 5, or 6 bits
                  bit_buf := bit_buf + (TRUNCATE_VALUE(copy_off, dict) shl bit_num);
                  bit_num := bit_num + dict;
              end;
          end;
      end;

      // If the copy length was less than two, include the byte as a literal byte
      if (max_copy_len < 2) then
      begin
          if (Ctype = IMPLODE_BINARY) then
          begin
              // Store a fixed size literal byte
              bit_buf := bit_buf + (ch shl (bit_num + 1));
              bit_num := bit_num + 9;
          end
          else
          begin
              // Store a variable size literal byte
              bit_buf := bit_buf + (s_ChCode[ch] shl (bit_num + 1));
              bit_num := bit_num + (1 + s_ChBits[ch]);
          end;

          // Add the byte into the dictionary
          dict_ptr^ := ch; Inc(dict_ptr);

          // If the dictionary is not full yet, increment the current dictionary size
          if (cur_dict_size < dict_size) then
            Inc(cur_dict_size);

          // If the current end of the dictionary is past the end of the buffer,
          // wrap around back to the start
          if (dict_ptr >= @dict_buf + dict_size) then
            dict_ptr := @dict_buf;
      end;

      // Write any whole bytes from the bit buffer into the output buffer
      while (bit_num >= 8) do
      begin
          // If output buffer has become full, stop immediately!
          if (wrt_ptr >= dest_end_ptr) then Exit;

          wrt_ptr^ := Byte(bit_buf); Inc(wrt_ptr);
          bit_buf := bit_buf shr 8;
          bit_num := bit_num - 8;
      end;
  end;

  // Store the code for the end of the compressed data stream
  bit_buf := bit_buf + ((1 + (s_LenCode[$0f] shl 1)) shl bit_num);
  bit_num := bit_num + (1 + s_LenBits[$0f]);

  bit_buf := bit_buf + ($ff shl bit_num);
  bit_num := bit_num + 8;

  // Write any remaining bits from the bit buffer into the output buffer
  while (bit_num > 0) do
  begin
      // If output buffer has become full, stop immediately!
      if (wrt_ptr >= dest_end_ptr) then Exit;

      wrt_ptr^ := Byte(bit_buf); Inc(wrt_ptr);
      bit_buf := bit_buf shr 8;
      if (bit_num >= 8) then
          bit_num := bit_num - 8
      else
          bit_num := 0;
  end;

  // Store the compressed size
  dest_size := wrt_ptr - dest;

  pklib_implode := true;
end;

function TDCL_Implode.pklib_explode(src: PByte; src_size: LongInt; dest: PByte; var dest_size: LongInt): Boolean;
var
  i: Integer;                                  // Index into tables
  copy_len: Integer;                   // Length of data to copy from the dictionary
  copy_off: PByte;                // Offset to data to copy from the dictionary
  Ctype: Byte;                              // Specifies whether to use fixed or variable size literal bytes
  dict: Byte;                              // Dictionary size; valid values are 4, 5, and 6 which represent 1024, 2048, and 4096 respectively
  rd_ptr: PByte;                 // Current position in input buffer
  wrt_ptr: PByte;                 // Current position in output buffer
  src_end_ptr: PByte;    // Pointer to the end of source buffer
  dest_end_ptr: PByte;    // Pointer to the end of dest buffer
  bit_num: Byte;                   // Number of bits in bit buffer
  bit_buf: LongInt;                  // Stores bits until there are enough to output a byte of data
  dict_ptr: PByte;                // Position in dictionary
  dict_size: LongInt;                 // Maximum size of dictionary
  cur_dict_size: LongInt;             // Current size of dictionary
  dict_buf: Array[0..4095] of Byte;  // Sliding dictionary used for compression and decompression
begin
  // Assume failure
  pklib_explode := false;
  
  // Compressed data cannot be less than 4 bytes;
  // this is not possible in any case whatsoever
  if (src_size < 4) then
  begin
    dest_size := 0;
    Exit;
  end;

  // Initialize buffer positions
  rd_ptr := src;
  wrt_ptr := dest;
  src_end_ptr := rd_ptr + src_size;
  dest_end_ptr := wrt_ptr + dest_size;

  // Get header from compressed data
  Ctype := rd_ptr^; Inc(rd_ptr);
  dict := rd_ptr^; Inc(rd_ptr);

  // Check for a valid compression type
  if ((Ctype <> IMPLODE_BINARY) AND (Ctype <> IMPLODE_ASCII)) then Exit;

  // Only dictionary sizes of 1024, 2048, and 4096 are allowed.
  // The values 4, 5, and 6 correspond with those sizes
  case (dict) of
    IMPLODE_DICT_1K:
      // Store actual dictionary size
      dict_size := 1024;
    IMPLODE_DICT_2K:
      // Store actual dictionary size
      dict_size := 2048;
    IMPLODE_DICT_4K:
      // Store actual dictionary size
      dict_size := 4096;
    else
      Exit;
  end;

  // Initialize dictionary position
  dict_ptr := @dict_buf;

  // Initialize current dictionary size to zero
  cur_dict_size := 0;

  // Get first 16 bits
  bit_buf := rd_ptr^; Inc(rd_ptr);
  bit_buf := bit_buf + (rd_ptr^ shl 8); Inc(rd_ptr);
  bit_num := 16;

  // Decompress until output buffer is full
  while (wrt_ptr < dest_end_ptr) do
  begin
    // Fill bit buffer with at least 16 bits
    while (bit_num < 16) do
    begin
      // If input buffer is empty before end of stream, buffer is incomplete
      if (rd_ptr >= src_end_ptr) then
      begin
        // Store the current size of output
        dest_size := wrt_ptr - dest;
        Exit;
      end;

      bit_buf := bit_buf + (rd_ptr^ shl bit_num); Inc(rd_ptr);
      bit_num := bit_num + 8;
    end;

    // First bit is 1; copy from dictionary
    if (bit_buf AND 1 <> 0) then
    begin
      // Remove first bit from bit buffer
      bit_buf := bit_buf shr 1;
      bit_num := bit_num - 1;

      // Find the base value for the copy length
      for i := 0 to $0F do
      begin
        if (TRUNCATE_VALUE(bit_buf, s_LenBits[i]) = s_LenCode[i]) then
          break;
      end;

      // Remove value from bit buffer
      bit_buf := bit_buf shr s_LenBits[i];
      bit_num := bit_num - s_LenBits[i];

      // Store the copy length
      copy_len := s_LenBase[i] + TRUNCATE_VALUE(bit_buf, s_ExLenBits[i]);

      // Remove the extra bits from the bit buffer
      bit_buf := bit_buf shr s_ExLenBits[i];
      bit_num := bit_num - s_ExLenBits[i];

      // If copy length is 519, the end of the stream has been reached
      if (copy_len = 519) then
        break;

      // Fill bit buffer with at least 14 bits
      while (bit_num < 14) do
      begin
        // If input buffer is empty before end of stream, buffer is incomplete
        if (rd_ptr >= src_end_ptr) then
        begin
          // Store the current size of output
          dest_size := wrt_ptr - dest;
          Exit;
        end;

        bit_buf := bit_buf + (rd_ptr^ shl bit_num); Inc(rd_ptr);
        bit_num := bit_num + 8;
      end;

      // Find most significant 6 bits of offset into the dictionary
      for i := 0 to $3f do
      begin
        if (TRUNCATE_VALUE(bit_buf, s_OffsBits[i]) = s_OffsCode[i]) then
          break;
      end;

      // Remove value from bit buffer
      bit_buf := bit_buf shr s_OffsBits[i];
      bit_num := bit_num - s_OffsBits[i];

      // If the copy length is 2, there are only two more bits in the dictionary
      // offset; otherwise, there are 4, 5, or 6 bits left, depending on what
      // the dictionary size is
      if (copy_len = 2) then
      begin
        // Store the exact offset to a byte in the dictionary
        // REETODO Pointer( was Ptr( before, is this right?
        copy_off := dict_ptr - 1 - ((i shl 2) + (bit_buf AND $03));

        // Remove the rest of the dictionary offset from the bit buffer
        bit_buf := bit_buf shr 2;
        bit_num := bit_num - 2;
      end
      else
      begin
        // Store the exact offset to a byte in the dictionary
        // REETODO Pointer( was Ptr( before, is this right?
        copy_off := dict_ptr - 1 - ((i shl dict) + TRUNCATE_VALUE(bit_buf, dict));

        // Remove the rest of the dictionary offset from the bit buffer
        bit_buf := bit_buf shr dict;
        bit_num := bit_num - dict;
      end;

      // While there are still bytes left, copy bytes from the dictionary
      while (copy_len > 0) do
      begin
        copy_len := copy_len - 1;

        // If output buffer has become full, stop immediately!
        if (wrt_ptr >= dest_end_ptr) then
        begin
          // Store the current size of output
          dest_size := wrt_ptr - dest;
          Exit;
        end;

        // Check whether the offset is a valid one into the dictionary
        while (copy_off < @dict_buf) do
          Inc(copy_off, cur_dict_size);
        while (copy_off >= @dict_buf + cur_dict_size) do
          Dec(copy_off, cur_dict_size);

        // Copy the byte from the dictionary and add it to the end of the dictionary
        dict_ptr^ := copy_off^; Inc(dict_ptr);
        wrt_ptr^ := copy_off^; Inc(wrt_ptr);
        Inc(copy_off);

        // If the dictionary is not full yet, increment the current dictionary size
        if (cur_dict_size < dict_size) then
          Inc(cur_dict_size);

        // If the current end of the dictionary is past the end of the buffer,
        // wrap around back to the start
        if (dict_ptr >= @dict_buf + dict_size) then
          dict_ptr := @dict_buf;
      end;
    end
    // First bit is 0; literal byte
    else
    begin
      // Fixed size literal byte
      if (Ctype = IMPLODE_BINARY) then
      begin
        // Copy the byte and add it to the end of the dictionary
        dict_ptr^ := Byte(bit_buf shr 1); Inc(dict_ptr);
        wrt_ptr^ := Byte(bit_buf shr 1); Inc(wrt_ptr);

        // Remove the byte from the bit buffer
        bit_buf := bit_buf shr 9;
        bit_num := bit_num - 9;
      end
      // Variable size literal byte
      else
      begin
        // Remove the first bit from the bit buffer
        bit_buf := bit_buf shr 1;
        bit_num := bit_num - 1;

        // Find the actual byte from the bit sequence
        for i := 0 to $ff do
        begin
          if (TRUNCATE_VALUE(bit_buf, s_ChBits[i]) = s_ChCode[i]) then
            break;
        end;

        // Copy the byte and add it to the end of the dictionary
        dict_ptr^ := i; Inc(dict_ptr);
        wrt_ptr^ := i; Inc(wrt_ptr);

        // Remove the byte from the bit buffer
        bit_buf := bit_buf shr s_ChBits[i];
        bit_num := bit_num - s_ChBits[i];
      end;

      // If the dictionary is not full yet, increment the current dictionary size
      if (cur_dict_size < dict_size) then
        Inc(cur_dict_size);

      // If the current end of the dictionary is past the end of the buffer,
      // wrap around back to the start
      if (dict_ptr >= @dict_buf + dict_size) then
        dict_ptr := @dict_buf;
    end;
  end;

  // Store the decompressed size
  dest_size := wrt_ptr - dest;

  pklib_explode := true;
end;

constructor TDCL_Implode.Create(InStream, OutStream: TStream);
begin
  FInStream := InStream;
  FOutStream := OutStream;
end;

//************************************************************************/

end.
