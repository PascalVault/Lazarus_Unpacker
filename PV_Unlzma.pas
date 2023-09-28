unit PV_Unlzma;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-27
//7zip LZMA .lzma

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

  { TUnPak }

type
  TUnlzma = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUntar }

constructor TUnlzma.Create(Str: TStream);
type THead = packed record
      Properties: Byte;
      DictionarySize: Cardinal;
      UnpackedSize: Int64;
    end;

var Head: THead;
    PackSize: Cardinal;
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    FStream.Read(Head, SizeOf(Head));

    PackSize := FStream.Size - FStream.Position;

    FFiles[FCount].Name := '??';
    FFiles[FCount].Offset := 0;//FStream.Position;
    FFiles[FCount].PackedSize := FStream.Size;//PackSize;

    if FCount = FSize then begin
      Inc(FSize, 1000);
      SetLength(FFiles, FSize);
    end;

    FFiles[FCount].UnpackedSize := Head.UnpackedSize;
    FFiles[FCount].PackMethod := pmLzma;

    Inc(FCount);

  except
  end;
end;

end.

