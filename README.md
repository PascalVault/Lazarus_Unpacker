# Lazarus_Unpacker
PV_Unpacker - simple pure Pascal library to unpack various archives (ZIP, RAR, LZMA, TAR...)

## Usage ###
    use PV_Unpacker;
    ...
    var Unp: TUnpacker;
    begin
      Unp := TUnpacker.Create(OpenDialog1.Filename);
    
      if Unp.GetFormat = '' then ShowMessage('Unsupported');
    
      for i:=0 to Unp.Count-1 do begin
        Memo1.Lines.Add( Unp.GetName(i) );
        if Unp.CanUnpack(i) then Unp.Extract(i, Unp.GetName(i) );
      end;
      Unp.Free;
    end;  
