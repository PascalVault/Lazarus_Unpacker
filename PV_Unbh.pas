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
       FileDate: Integer;
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
begin
  FStream := Str;

  try
    FSize  := 1000;
    SetLength(FFiles, FSize);
    FCount := 0;

    while True do begin
      FStream.Read(Head, SizeOf(Head));

      SetLength(FName, Head.FNameLen);
      FStream.Read(FName[1], Head.FNameLen);

      if Head.CommentLen > 0 then begin
        SetLength(Comment, Head.CommentLen);
        FStream.Read(Comment[1], Head.CommentLen);
      end;

      if FCount = FSize then begin
        Inc(FSize, 1000);
        SetLength(FFiles, FSize);
      end;

      //showmessage(FName);

      FFiles[FCount].Name := FName;
      FFiles[FCount].Offset := FStream.Position;
      FFiles[FCount].PackedSize := Head.PackedSize;
      FFiles[FCount].UnpackedSize := Head.UnpackedSize;

      case Head.Compression of
        0  : FFiles[FCount].PackMethod := pmStore;
        8  : FFiles[FCount].PackMethod := pmDeflate;
        else FFiles[FCount].PackMethod := pmOther;
      end;

      FStream.Position := FStream.Position + Head.PackedSize;
      Inc(FCount);

      if FStream.Position > FStream.Size-1 then break;
    end;
  except
  end;
end;

end.

