unit PV_Unzip;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-28
//PK ZIP (store, deflate, lzma, bzip2)

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

type
  { TUnzip }

  TUnzip = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUnzip }

constructor TUnzip.Create(Str: TStream);
type THead = packed record
       Magic: Cardinal;
       MinimumVersion: Word;
       GeneralFlag: Word;
       Compression: Word;
       ModTime: Word;
       ModDate: Word;
       CRC32: Cardinal;
       PackedSize: Cardinal;
       UnpackedSize: Cardinal;
       FNameLen: Word;
       ExtraLen: Word;
    end;

var Head: THead;
    FName: String;

    LzmaMajorVersion: Byte;
    LzmaMinorVersion: Byte;
    LzmaPropLen: Word;
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    while True do begin
      FStream.Read(Head, SizeOf(Head));

      if Head.Magic <> $04034b50 then break;

      SetLength(FName, Head.FNameLen);
      FStream.Read(FName[1], Head.FNameLen);
      FStream.Position := FStream.Position + Head.ExtraLen;

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      if Head.Compression = 14 then begin
        FStream.Read(LzmaMajorVersion, 1);
        FStream.Read(LzmaMinorVersion, 1);
        FStream.Read(LzmaPropLen, 2);
      end;

      FFiles[FCount].Name := FName;
      FFiles[FCount].Offset := FStream.Position;
      FFiles[FCount].PackedSize := Head.PackedSize;
      FFiles[FCount].UnpackedSize := Head.UnpackedSize;

      case Head.Compression of
        0  : FFiles[FCount].PackMethod := pmStore;
        8  : FFiles[FCount].PackMethod := pmDeflate;
        12 : FFiles[FCount].PackMethod := pmBzip2;
        14 : FFiles[FCount].PackMethod := pmLzma;
        else FFiles[FCount].PackMethod := pmOther;
      end;

      FStream.Position := FStream.Position + Head.PackedSize;
      Inc(FCount);
    end;
  except
  end;
end;

end.

