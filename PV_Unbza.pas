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
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    while True do begin
      FStream.Read(Head, SizeOf(Head));

      SetLength(FName, Head.DirLen+Head.FNameLen);
      FStream.Read(FName[1], Head.DirLen+Head.FNameLen);


      if FCount = FSize then begin
         Inc(FSize, 1000);
         SetLength(FFiles, FSize);
      end;

      FFiles[FCount].Name := FName;
      FFiles[FCount].Offset := FStream.Position;
      FFiles[FCount].PackedSize := Head.PackedSize;
      FFiles[FCount].UnpackedSize := Head.UnpackedSize;

      if Head.Magic = 'GZIP' then FFiles[FCount].PackMethod := pmDeflate //TODO
      else                        FFiles[FCount].PackMethod := pmBzip2;

      FStream.Position := FStream.Position + Head.PackedSize;
      Inc(FCount);

      if FStream.Position+1024 >= FStream.Size then break;
    end;

  except
  end;
end;

end.

