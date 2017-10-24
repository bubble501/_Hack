unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Jsons;

type
  TForm1 = class(TForm)
    btnPack: TButton;
    btnUnPack: TButton;
    btnSpeed: TButton;
    procedure btnPackClick(Sender: TObject);
    procedure btnUnPackClick(Sender: TObject);
    procedure btnSpeedClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnPackClick(Sender: TObject);
var
  Json: TJson;
  Str: String;
begin
  Json := TJson.Create;

  try

    Json.Put('null-field', null);
    Json.Put('boolean-field-true', True);

    Json['boolean-field-false'].AsBoolean := not Json.Get('boolean-field-true').AsBoolean;
    Json['number-field'].AsNumber := 3.1415926535;
    Json['number-field-integer'].AsInteger := Json['number-field'].AsInteger;
    Json['string-field'].AsString := 'Hello world';

    with Json.Put('array-field', empty).AsArray do
    begin
      Put(empty);
      Put(null);
      Put(False);
      Put(True);
      Put(299792458);
      Put(2.7182818284);
      Put('The magic words are squeamish ossifrage');
      with Put(empty).AsObject do
      begin
        Put('array-object-field-1', null);
        Put('array-object-field-2', 'json4delphi');
      end;
    end;
    with Json.Put('object-field', empty).AsObject do
    begin
      Put('object-field-1', True);
      Put('object-field-2', 6.6260755e-34);
    end;
    Str := Json.Stringify;

    ShowMessage(Str);
    Json.Clear;
  finally
    Json.Free;
  end;
end;

procedure TForm1.btnUnPackClick(Sender: TObject);
var
  Str: string;
  Json: TJson;
  show: string;
begin
  Str := '{"null-field":null,"boolean-field-true":true,"boolean-field-false":false,"number-field":3.1415926535,'
      +     '"number-field-integer":3,"string-field":"Hello world",'
      +  '"array-field":[null,null,false,true,299792458,2.7182818284,"The magic words are squeamish ossifrage",'
      +     '["xumenger",123456],'
      +     '{"array-object-field-1":null,"array-object-field-2":"json4delphi"}],'
      +  '"object-field":{"object-field-1":true,"object-field-2":6.6260755E-34}}';

  Json := TJson.Create;
  try
    Json.Parse(Str);

    show := 'null-field = ' + Json['null-field'].AsString + #13#10;
    show := show + 'boolean-field-true = ' + Json['boolean-field-true'].AsString + #13#10;
    show := show + 'string-field = ' + Json['string-field'].AsString + #13#10;
    show := show + 'array-field[6] = ' + Json['array-field'].AsArray[6].AsString + #13#10;
    show := show + 'array-field[7][0] = ' + Json['array-field'].AsArray[7].AsArray[0].AsString + #13#10;
    show := show + 'array-field[7][array-object-field-2] = ' + Json['array-field'].AsArray[8].AsObject['array-object-field-2'].AsString + #13#10;
    show := show + 'object-field[object-field-2] = ' + Json['object-field'].AsObject['object-field-2'].AsString;

    ShowMessage(show);
    Json.Clear;
  finally
    Json.Free;
  end;
end;

procedure TForm1.btnSpeedClick(Sender: TObject);
var
  Str: string;
  Json: TJson;
  show: string;
  BeginTime : Cardinal;
  times: Integer;  
begin
  Str := '{"null-field":null,"boolean-field-true":true,"boolean-field-false":false,"number-field":3.1415926535,'
      +     '"number-field-integer":3,"string-field":"Hello world",'
      +  '"array-field":[null,null,false,true,299792458,2.7182818284,"The magic words are squeamish ossifrage",'
      +     '{"array-object-field-1":null,"array-object-field-2":"json4delphi"}],'
      +  '"object-field":{"object-field-1":true,"object-field-2":6.6260755E-34}}';

  times := 0;
  BeginTime := GetTickCount;
  while((GetTickCount - BeginTime) < 6000) do
  begin
    Json := TJson.Create;
    try
      Json.Parse(Str);

      show := 'null-field = ' + Json['null-field'].AsString + #13#10;
      show := show + 'boolean-field-true = ' + Json['boolean-field-true'].AsString + #13#10;
      show := show + 'string-field = ' + Json['string-field'].AsString + #13#10;
      show := show + 'array-field[6] = ' + Json['array-field'].AsArray[6].AsString + #13#10;
      show := show + 'object-field[object-field-2] = ' + Json['object-field'].AsObject['object-field-2'].AsString;

      Inc(times);
      Json.Clear;
    finally
      Json.Free;
    end;
  end;

  ShowMessage('6sÖÐ½âÎö ' + IntToStr(times) + ' ´Î');
end;

end.
