unit PV_Unzoo;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-20
//.ZOO

interface

uses
  Classes, SysUtils, PV_Unpack, CRC16_ARC, Dialogs;

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
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;
  FHasherClass := THasherCRC16_ARC;

  try
    FStream.Read(Head, SizeOf(Head));

    FStream.Position := Head.OffsetFile;

    while FStream.Position < FStream.Size do begin

      FStream.Read(Entry, SizeOf(Entry));

      if Entry.NextHeadOffset = 0 then break;

      AFile.Name := Entry.FName;
      AFile.Offset := Entry.NextHeadOffset;
      AFile.PackedSize := Entry.PackedSize;
      AFile.UnpackedSize := Entry.UnpackedSize;
      AFile.ModDate := Dos2DateTime(Entry.Time, Entry.Date);
      AFile.CRC32 := Entry.CRC16;

      if Entry.Compression = 0      then AFile.PackMethod := pmStore
      else if Entry.Compression = 2 then AFile.PackMethod := pmLH5
      else                               AFile.PackMethod := pmOther;

      AddFile(AFile);

      FStream.Position := Entry.NextEntryOffset;
    end;
  except
  end;
end;

end.

