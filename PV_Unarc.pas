unit PV_Unarc;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-21
//Thom Henderson .ARC

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnarc = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

//Methods:
//0 - EOF
//1 - unpacked (obsolete)
//2 - unpacked
//3 - RLE
//4 - squeezed (after packing)
//5 - crunched (obsolete)
//6 - crunched (after packing) (obsolete)
//7 - crunched (after packing, using faster hash algorithm)
//8 - crunched (after packing, using dynamic LZW variations)
//9 - Squashed c/o Phil Katz (no packing)

constructor TUnarc.Create(Str: TStream);
type THead = packed record
       Magic: Byte; //1A
       Method: Byte;
     end;
     TEntry = packed record
       FName: array[0..12] of Char;    //max 12 bytes+null
       PackedSize: Cardinal;
       FileTime: Word;
       FileDate: Word;
       CRC16: Word;
       UnpackedSize: Cardinal;
    end;

var Entry: TEntry;
    Head: THead;
    i: Integer;
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;


    while FStream.Position < FStream.Size do begin

      FStream.Read(Head, SizeOf(Head));

      if (Head.Magic <> $1A) or (Head.Method = 0) then break;

      FStream.Read(Entry, SizeOf(Entry));

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := Entry.FName;
      FFiles[FCount].Offset := FStream.Position;
      FFiles[FCount].PackedSize := Entry.PackedSize;
      FFiles[FCount].UnpackedSize := Entry.UnpackedSize;

      if Head.Method = 2 then FFiles[FCount].PackMethod := pmStore
      else                    FFiles[FCount].PackMethod := pmOther;

      Inc(FCount);

      FStream.Position := FStream.Position + Entry.PackedSize;
    end;

  except
  end;
end;

end.

