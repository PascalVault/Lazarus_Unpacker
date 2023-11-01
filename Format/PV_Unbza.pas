unit PV_Unbza;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-18
//BGA format = .BZA

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnbza = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnbza.Create(Str: TStream);
type THead = packed record
      Checksum: Cardinal;
      Magic: array[0..3] of Char;
      PackedSize: Cardinal;
      UnpackedSize: Cardinal;
      Date: Word;
      Time: Word;
      Attrib: Byte;
      HeadType: Byte;
      ArcType: Word;
      DirLen: Word;
      FNameLen: Word;
    end;


var Head: THead;
    i: Integer;
    FName: String;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    while True do begin
      FStream.Read(Head, SizeOf(Head));

      SetLength(FName, Head.DirLen+Head.FNameLen);
      FStream.Read(FName[1], Head.DirLen+Head.FNameLen);

      AFile.Name := FName;
      AFile.Offset := FStream.Position;
      AFile.PackedSize := Head.PackedSize;
      AFile.UnpackedSize := Head.UnpackedSize;

      if Head.Magic = 'GZIP' then AFile.PackMethod := pmDeflate //TODO
      else                        AFile.PackMethod := pmBzip2;

      AFile.ModDate := Dos2DateTime(Head.Time, Head.Date);
      AFile.CRC32 := 0;

      AddFile(AFile);

      FStream.Position := FStream.Position + Head.PackedSize;

      if FStream.Position+1024 >= FStream.Size then break;
    end;

  except
  end;
end;

end.

