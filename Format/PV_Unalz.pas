unit PV_Unalz;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-19
//Alzip .ALZ

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnalz = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnalz.Create(Str: TStream);
type TEntry = packed record
       Magic: array[1..3] of Char;
       Version: Byte;
       Unk1: Word;
       Unk2: Byte;
       Unk3: Byte;
       BLZ: array[1..3] of Char;
       Version2: Byte;
       FNameLength: Word;
       FileAttrib: Byte;
       FileModTime: Word;
       FileModDate: Word;
       BytesForSize: Word;
       Compression: Byte;
       Unk4: Byte;
       CRC32: Cardinal;
     end;

var Entry: TEntry;
    i: Integer;
    FName: String;
    PackedSize: Cardinal;
    UnpackedSize: Cardinal;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try

    while FStream.Position < FStream.Size do begin

      FStream.Read(Entry, SizeOf(Entry));

      PackedSize := 0;
      UnpackedSize := 0;

      if Entry.BytesForSize = $10 then begin
        FStream.Read(PackedSize, 1);
        FStream.Read(UnpackedSize, 1);
      end
      else if Entry.BytesForSize = $20 then begin
        FStream.Read(PackedSize, 2);
        FStream.Read(UnpackedSize, 2);
      end
      else if Entry.BytesForSize = $40 then begin
        FStream.Read(PackedSize, 4);
        FStream.Read(UnpackedSize, 4);
      end;

      if PackedSize = 0 then break;

      SetLength(FName, Entry.FNameLength);
      FStream.Read(FName[1], Entry.FNameLength);

      AFile.Name := FName;
      AFile.Offset := FStream.Position;
      AFile.PackedSize := PackedSize;
      AFile.UnpackedSize := UnpackedSize;

      if Entry.Compression = 0 then AFile.PackMethod := pmStore
      else                          AFile.PackMethod := pmOther;

      AFile.ModDate := Dos2DateTime(Entry.FileModTime, Entry.FileModDate);
      AFile.CRC32 := Entry.CRC32;

      AddFile(AFile);

      FStream.Position := FStream.Position + PackedSize;
    end;
  except
  end;
end;

end.

