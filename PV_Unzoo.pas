unit PV_Unzoo;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-20
//.ZOO

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnzoo = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnzoo.Create(Str: TStream);
type THead = packed record
       HeadText: array[0..19] of Char; //^Z terminated, null padded
       Magic: Cardinal; //FDC4A7DC
       OffsetFile: Cardinal;
       OffsetMinus: Cardinal;
       MajorVersion: Byte;
       MinorVersion: Byte;
    end;
    TEntry = packed record
      Magic: Cardinal;
      FileType: Byte;
      Compression: Byte; //0=store
      NextEntryOffset: Cardinal;
      NextHeadOffset: Cardinal;
      Date: Word;
      Time: Word;
      CRC16: Word;            //CRC-16 ARC
      UnpackedSize: Cardinal;
      PackedSize: Cardinal;
      MajorVersion: Byte;
      MinorVersion: Byte;
      Deleted: Byte; //1=deleted, 0=normal
      Struc: Byte;
      CommentOffset: Cardinal; //0 for none
      CommentLen: Word;
      FName: array[0..12] of Char; //ASCIIZ

      VarDirSize: Byte;
      TimeZone: Byte;
      DirCRC: Cardinal;
    end;


var Head: THead;
    Entry: TEntry;
    i: Integer;

begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    FStream.Read(Head, SizeOf(Head));

    FStream.Position := Head.OffsetFile;

    while FStream.Position < FStream.Size do begin

      FStream.Read(Entry, SizeOf(Entry));

      if Entry.NextHeadOffset = 0 then break;

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := Entry.FName;
      FFiles[FCount].Offset := Entry.NextHeadOffset;
      FFiles[FCount].PackedSize := Entry.PackedSize;
      FFiles[FCount].UnpackedSize := Entry.UnpackedSize;

      if Entry.Compression = 0 then FFiles[FCount].PackMethod := pmStore
      else                          FFiles[FCount].PackMethod := pmOther;

      FStream.Position := Entry.NextEntryOffset;

      Inc(FCount);
    end;
  except
  end;
end;

end.

