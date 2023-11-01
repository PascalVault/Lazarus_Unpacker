unit PV_Unbh;

{$mode objfpc}{$H+}

//PV Unpack
//https://github.com/PascalVault
//Licence: MIT
//Last update: 2023-09-16
//BlakHole .BH

interface

uses
  Classes, SysUtils, PV_Unpack, Dialogs;

type
  { TUnbh }

  TUnbh = class(TUnpack)
  public
    constructor Create(Str: TStream); override;
  end;


implementation

{ TUnbh }

constructor TUnbh.Create(Str: TStream);
type THead = packed record
       SignAtr: Integer;
       HdrSize: Word;
       HeadSize: Byte;        	//not used
       VerNum: Byte;
       MinVerNum: Byte;
       BitFlag: Byte;
       Compression: Byte;       //0= store, 8=deflate
       FileTime: Word;
       FileDate: Word;
       PackedSize: Cardinal;
       UnpackedSize: Cardinal;
       CRC32: Cardinal;
       ExternalAttr: Word;
       HeadCrc32: Cardinal;
       FNameLen: Word;
       CommentLen: Word;
    end;

var Head: THead;
    FName: String;
    Comment: String;
    AFile: TFile;
begin
  inherited Create(Str);

  FStream := Str;

  try

    while True do begin
      FStream.Read(Head, SizeOf(Head));

      SetLength(FName, Head.FNameLen);
      FStream.Read(FName[1], Head.FNameLen);

      if Head.CommentLen > 0 then begin
        SetLength(Comment, Head.CommentLen);
        FStream.Read(Comment[1], Head.CommentLen);
      end;

      AFile.Name := FName;
      AFile.Offset := FStream.Position;
      AFile.PackedSize := Head.PackedSize;
      AFile.UnpackedSize := Head.UnpackedSize;

      case Head.Compression of
        0  : AFile.PackMethod := pmStore;
        8  : AFile.PackMethod := pmDeflate;
        else AFile.PackMethod := pmOther;
      end;

      AFile.ModDate := Dos2DateTime(Head.FileTime, Head.FileDate);
      AFile.CRC32 := Head.CRC32;

      AddFile(AFile);

      FStream.Position := FStream.Position + Head.PackedSize;

      if FStream.Position > FStream.Size-1 then break;
    end;
  except
  end;
end;

end.

