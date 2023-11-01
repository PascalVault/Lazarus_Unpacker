unit PV_Unace;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-10-21
//Marcel Lemke .ACE 1.2

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnace = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnace.Create(Str: TStream);
type THead = packed record
       CRC: Word;
       Size: Word;
       Typee: Byte;
       Flags: Word;

       Magic: array[0..6] of Char; //**ACE**
       VersionExtract: Byte;
       VersionCreate: Byte;
       Host: Byte;
       VolumeNum: Byte;
       FileTime: Word;
       FileDate: Word;
       Reserved1: Cardinal;
       Reserved2: Cardinal;
     end;

     TEntry = packed record
       CRC: Word;
       Size: Word;
       Typee: Byte;
       Flags: Word;

       PackedSize: Cardinal;
       UnpackedSize: Cardinal;
       FileTime: Word;
       FileDate: Word;
       Attrib: Cardinal;
       CRC32: Cardinal;
       Method: Byte;
       Speed: Byte;
       Decompression: Word;
       Reserved: Word;
       FNameLen: Word;
       //FName
    end;

var Entry: TEntry;
    AV: String;
    AvLen: Byte;
    Head: THead;
    i: Integer;
    FName: String;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    while FStream.Position < FStream.Size do begin

      FStream.Read(Head, SizeOf(Head));
      FStream.Read(AvLen, 1);

      SetLength(AV, AVLen);
      FStream.Read(AV[1], AvLen);

      FStream.Read(Entry, SizeOf(Entry));

      SetLEngth(FName, Entry.FNameLen);
      FStream.Read(FName[1], Entry.FNameLen);

      AFile.Name := FName;
      AFile.Offset := FStream.Position;
      AFile.PackedSize := Entry.PackedSize;
      AFile.UnpackedSize := Entry.UnpackedSize;
      AFile.ModDate := Dos2DateTime(Entry.FileTime, Entry.FileDate);
      AFile.CRC32 := Entry.CRC32;

      if Entry.Method = 0 then AFile.PackMethod := pmStore
      else                     AFile.PackMethod := pmOther;

      AddFile(AFile);

      FStream.Position := FStream.Position + Entry.PackedSize;
    end;

  except
  end;
end;

end.

