unit PV_Unlzx;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-19
//Amiga .LZX

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnlzx = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnlzx.Create(Str: TStream);
type THead = packed record
      Magic: array[0..2] of Char; //"LZX"
      Flags: Byte;
      Unknown: Cardinal;
      Unk1: Word;
    end;
    TEntry = packed record
      Attrib: Byte;
      Unk: Byte;
      UnpackedSize: Cardinal;
      PackedSize: Cardinal;
      Machine: Byte;
      PackMode: Byte; //0=store, 2=lzx
      Flags2: Word;
      LenComment: Byte;
      ExtractVersion: Byte;
      Unk3: Byte;
      Unk4: Byte;
      FileTimeDate: Cardinal;
      CRC32: Cardinal;
      HeadCRC: Cardinal;
      FNameLen: Byte;
    end;
    //31 bytes

var Head: THead;
    Entry: TEntry;
    i: Integer;
    FName: String;
    Comment: String;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    FStream.Read(Head, SizeOf(Head));

    while FStream.Position < FStream.Size do begin

      FStream.Read(Entry, SizeOf(Entry));

      SetLength(FName, Entry.FNameLen);
      FStream.Read(FName[1], Entry.FNameLen);

      if Entry.LenComment > 0 then begin
        SetLength(Comment, Entry.LenComment);
        FStream.Read(Comment[1], Entry.LenComment);
      end;

      AFile.Name := FName;
      AFile.Offset := FStream.Position;
      AFile.PackedSize := Entry.PackedSize;
      AFile.UnpackedSize := Entry.UnpackedSize;

      if Entry.PackMode = 0 then AFile.PackMethod := pmStore
      else                       AFile.PackMethod := pmOther;

      FStream.Position := FStream.Position + Entry.PackedSize;

      AFile.ModDate := Amiga2DateTime(Entry.FileTimeDate);
      AFile.CRC32 := Entry.CRC32;

      AddFile(AFile);
    end;
  except
  end;
end;

end.

