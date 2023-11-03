unit preview_box;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TPreviewForm }

  TPreviewForm = class(TForm)
    Memo1: TMemo;
  private

  public

  end;

var
  PreviewForm: TPreviewForm;

implementation

{$R *.lfm}

end.

