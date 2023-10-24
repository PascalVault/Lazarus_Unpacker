unit DLTUnpack;
{$mode objfpc}

// Stargunner .DLT decompressor
// Based on code by The_coder and Adam Nielsen
// Translated to Pascal: github.com/PascalVault

interface

uses Classes;

function Unpack_DLT(InStream, OutStream: TStream): Boolean;

implementation

function DecodeChunk(const inn: array of Byte; expanded_size: Integer; var outt: array of Byte): Cardinal;
var tableA, tableB: array[0..255] of Byte;
    inpos, outpos: Cardinal;
    i: Integer;
    code: Byte;
    tablepos: Cardinal;
    len: Integer;
    expbufpos: Integer;
    expbuf: array[0..31] of Byte; // This is the maximum number of bytes a single codeword can expand to.
    data: Byte;
    codeword: Integer;
begin
  inpos := 0;
  outpos := 0;

    while (outpos < expanded_size) do begin
      // Initialise the dictionary so that no bytes are codewords (or if you
      // prefer, each byte expands to itself only.)
      for i:=0 to 255 do tableA[i] := i;

      //
      // Read in the dictionary
      //

      tablepos := 0;
      while (tablepos < 256) do begin
        code := inn[inpos];
        Inc(inpos);

        // If the code has the high bit set, the lower 7 bits plus one is the
        // number of codewords that will be skipped from the dictionary.  (Those
        // codewords were initialised to expand to themselves in the loop above.)
        if (code > 127) then begin
          Inc(tablepos, code - 127);
          code := 0;
        end;
        if (tablepos = 256) then break;

        // Read in the indicated number of codewords.
        for i :=0 to code do begin
          assert(tablepos < 256);
          data := inn[inpos];
          Inc(inpos);
          tableA[tablepos] := data;

          if (tablepos <> data) then begin
            // If this codeword didn't expand to itself, store the second byte
            // of the expansion pair.
            tableB[tablepos] := inn[inpos];
            Inc(inpos);
          end;
          Inc(tablepos);
        end;
      end;

      // Read the length of the data encoded with this dictionary
      len := inn[inpos];
      Inc(inpos);
      len := len or (inn[inpos] shl 8);
      Inc(inpos);

      //
      // Decompress the data
      //

      expbufpos := 0;
      while true do begin
        if (expbufpos <> 0) then begin
          // There is data in the expansion buffer, use that
          Dec(expbufpos);
          code := expbuf[expbufpos];
        end
        else begin
          // There is no data in the expansion buffer, use the input data
          Dec(len);
          if (len = -1) then break; // no more input data
          code := inn[inpos];
          Inc(inpos);
        end;

        if (code = tableA[code]) then begin
          // This byte is itself, write this to the output
          outt[outpos] := code;
           Inc(outpos);
        end
        else begin
          // This byte is actually a codeword, expand it into the expansion buffer
          codeword := sizeof(expbuf) - 2;
          assert(expbufpos < codeword);
          expbuf[expbufpos] := tableB[code];
          Inc(expbufpos);
          expbuf[expbufpos] := tableA[code];
          Inc(expbufpos);
        end;
      end;
    end;
    Result := outpos - expanded_size;
end;


function Unpack_DLT(InStream, OutStream: TStream): Boolean;
const CHUNK_SIZE = 4096;
      PAK_CHUNK_SIZE = (CHUNK_SIZE + 256);

var InBuf, OutBuf: array of Byte;
    OutSize: Cardinal;
    ChunkLen: Word;
    LenOut: Integer;
    TotalOut: Cardinal;
begin
  Result := True;

  InStream.Read(OutSize, 4);
  TotalOut := 0;

  SetLength(InBuf, PAK_CHUNK_SIZE);
  SetLength(OutBuf, CHUNK_SIZE);

  while TotalOut < OutSize do begin
    InStream.Read(ChunkLen, 2);

    if ChunkLen > PAK_CHUNK_SIZE then Exit(False);

    InStream.Read(InBuf[0], ChunkLen);

    if (OutSize < CHUNK_SIZE) then LenOut := OutSize
    else                           LenOut := CHUNK_SIZE;

    DecodeChunk(InBuf, LenOut, OutBuf);

    OutStream.Write(OutBuf[0], LenOut);
    Inc(TotalOut, LenOut);
  end;
end;


end.

