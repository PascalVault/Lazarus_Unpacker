unit PV_Unzip;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-28
//PK ZIP (store, deflate, lzma, bzip2)

interface

uses
  Classes, SysUtils, PV_Unpack, CRC32_ISOHDLC, Dialogs;

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
    AFile: TFile;
    LzmaMajorVersion: Byte;
    LzmaMinorVersion: Byte;
    LzmaPropLen: Word;
begin
  inherited Create(Str);

  FStream := Str;
  FHasherClass := THasherCRC32_ISOHDLC;

  try

    while True do begin
      FStream.Read(Head, SizeOf(Head));

      if Head.Magic <> $04034b50 then break;

      SetLength(FName, Head.FNameLen);
      FStream.Read(FName[1], Head.FNameLen);
      FStream.Position := FStream.Position + Head.ExtraLen;

      if Head.Compression = 14 then begin
        FStream.Read(LzmaMajorVersion, 1);
        FStream.Read(LzmaMinorVersion, 1);
        FStream.Read(LzmaPropLen, 2);
      end;

      AFile.Name := FName;
      AFile.Offset := FStream.Position;
      AFile.PackedSize := Head.PackedSize;
      AFile.UnpackedSize := Head.UnpackedSize;
      AFile.ModDate := Dos2DateTime(Head.ModTime, Head.ModDate);
      AFile.CRC32 := Head.CRC32;

//if Head.GeneralFlag and 1 = 1 then 
//AFile.Encryption := enZipCrypto;

      case Head.Compression of
        0  : AFile.PackMethod := pmStore;
        1  : AFile.PackMethod := pmShrink;

        2  : AFile.PackMethod := pmReduce1;
        3  : AFile.PackMethod := pmReduce2;
        4  : AFile.PackMethod := pmReduce3;
        5  : AFile.PackMethod := pmReduce4;

        6  : AFile.PackMethod := pmImplode2;
        8  : AFile.PackMethod := pmDeflate;
        10 : AFile.PackMethod := pmDCLImplode;
        12 : AFile.PackMethod := pmBzip2;
        14 : AFile.PackMethod := pmLzma;
        else AFile.PackMethod := pmOther;
      end;

      if AFile.PackMethod = pmImplode2 then begin
        if (Head.GeneralFlag and $02) <> 0 then AFile.Extra := 8
        else                                    AFile.Extra := 4;

        if (Head.GeneralFlag and $04) <> 0 then AFile.PackMethod := pmImplode3;
      end;

      AddFile(AFile);

      FStream.Position := FStream.Position + Head.PackedSize;
    end;
  except
  end;
end;

end.

