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
      Unk3: Word;
      FileTimeDate: Cardinal;
      CRC32: Cardinal;
      HeadCRC: Cardinal;
      FNameLen: Byte;
    end;
    //DateTime = 4 bytes
    //bit 0 is MSB, 31 is LSB
    //bit # 0-4=Day 5-8=Month 9-14=Year 15-19=Hour 20-25=Minute 26-31=Second

var Head: THead;
    Entry: TEntry;
    i: Integer;
    FName: String;
    Comment: String;
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    FStream.Read(Head, SizeOf(Head));

    while FStream.Position < FStream.Size do begin

      FStream.Read(Entry, SizeOf(Entry));

      SetLength(FName, Entry.FNameLen);
      FStream.Read(FName[1], Entry.FNameLen);

      if Entry.LenComment > 0 then begin
        SetLength(Comment, Entry.LenComment);
        FStream.Read(Comment[1], Entry.LenComment);
      end;

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := FName;
      FFiles[FCount].Offset := FStream.Position;
      FFiles[FCount].PackedSize := Entry.PackedSize;
      FFiles[FCount].UnpackedSize := Entry.UnpackedSize;

      if Entry.PackMode = 0 then FFiles[FCount].PackMethod := pmStore
      else                       FFiles[FCount].PackMethod := pmOther;

      FStream.Position := FStream.Position + Entry.PackedSize;

      Inc(FCount);
    end;
  except
  end;
end;

end.

