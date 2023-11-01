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
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try
    FStream.Read(Head, SizeOf(Head));

    PackSize := FStream.Size - FStream.Position;

    AFile.Name := '';
    AFile.Offset := 0;//FStream.Position;
    AFile.PackedSize := FStream.Size;//PackSize;

    AFile.UnpackedSize := Head.UnpackedSize;
    AFile.PackMethod := pmLzma;
    AFile.ModDate := 0;
    AFile.CRC32 := 0;

    AddFile(AFile);

  except
  end;
end;

end.

