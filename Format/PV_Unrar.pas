unit PV_Unrar;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-16

interface

uses
  Classes, SysUtils, PV_Unpack;

type
  { TUnrar }

  TUnrar = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUnrar }

constructor TUnrar.Create(Str: TStream);
type TVolHead = packed record
       CRC: Word;
       Typee: Byte;
       Flags: Word;
       Size: Word;
     end;

     THead = packed record
       PackedSize: Cardinal;
       UnpackedSize: Cardinal;
       HostOS: Byte;
       CRC32: Cardinal;
       FileTime: Word;
       FileDate: Word;
       Version: Byte;
       Method: Byte;
       FNameLen: Word;
       FileAttr: Cardinal;
     end;

var Head: THead;
    Vol: TVolHead;
    FName: String;
    Pos: Integer;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try

    while True do begin

      FStream.Read(Vol, SizeOf(Vol));

      if Vol.Typee = $74 then begin
        Pos := FStream.Position;

        FStream.Read(Head, SizeOf(Head));

        SetLength(FName, Head.FNameLen);
        FStream.Read(FName[1], Head.FNameLen);

        FStream.Position := Pos + Vol.Size - 7;

        AFile.Name := FName;
        AFile.Offset := FStream.Position;
        AFile.PackedSize := Head.UnPackedSize;
        AFile.UnpackedSize := Head.UnpackedSize;
        AFile.ModDate := Dos2DateTime(Head.FileTime, Head.FileDate);
        AFile.CRC32 := Head.CRC32;

        if Head.Method = 48 then AFile.PackMethod := pmStore
        else                     AFile.PackMethod := pmOther;

        AddFile(AFile);

        FStream.Position := FStream.Position + Head.PackedSize;
      end
      else begin
        FStream.Position := FStream.Position + Vol.Size - 7;
      end;

      if FStream.Position >= FStream.Size then break;
    end;
  except
  end;
end;

end.

