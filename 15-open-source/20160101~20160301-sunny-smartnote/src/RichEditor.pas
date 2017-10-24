////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   RichEditor.pas
//  Creator     :   Shen Min
//  Date        :   2002-07-02
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit RichEditor;

interface

uses ComCtrls, Controls, Classes, Graphics, Dialogs, StdCtrls, SysUtils,
     Editor, IntfEditor;

type
    {前面使用TMemo实现了IssnEditor接口，可是某一天你的经理可能会对你说，TMemo太土了，在Win9x平台下，TMemo无法承载超过64KB的文本，怎么办？
      只好重新实现一个。
      不过，幸好我们已经为这样的动作做好了准备，这也是TssnEditor存在的原因。
      于是便可以心平气和地遵从经理的意见
      很显然，TRichEdit可以打开超过64KB的文件
        虽然TRichEdit是为富文本文件编辑器所准备的，不过只需要将它的PlainText属性设置为True，便可以用于处理纯文本文件
      }
    {TssnRichEditor的声明和TssnMemoEditor非常类似，因为它们都是实现TssnEditor所规定的接口
      不同之处在于TssnRichEditor内部包含的是一个TRichEdit控件的实例
      另外，TRichEdit与TMemo由于事件接口有所不同，因此事件处理也不一样
      }
    {至于具体的实现，与TssnMemoEditor的区别仅仅在于所基于的组件不同。
      也就是说TssnRichEditor与TssnMemoEditor都是同种性质的所谓“接口适配器”
      只不过，一个是将TMemo的接口转换成TssnEditor，一个是将TRichEdit的接口转换成TssnEditor
      }
    TssnRichEditor = class(TssnEditor)
    private
        m_Edit : TRichEdit;   //内部包含一个TRichEdit实例，TssnRichEditor只是对它进行接口改造

    protected
        procedure DoLoadFromFile(FileName : String); override;

        procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

        function GetText() : String; override;

    public
        constructor Create(ParentCtrl : TWinControl);
        destructor Destroy(); override;

        procedure SaveToFile(FileName: String); override;

        // GetFileName
        function GetSaved() : Boolean; override;
        // Save
        // SaveAs
        function GetSelectText() : String; override;
        procedure SetFont(Font : TFont); override;
        procedure Undo(); override;
        function CanUndo() : Boolean; override;
        procedure Redo(); override;
        function CanRedo() : Boolean; override;
        procedure Cut(); override;
        function CanCut() : Boolean; override;
        procedure Copy(); override;
        function CanCopy() : Boolean; override;
        procedure Paste(); override;
        function CanPaste() : Boolean; override;
        procedure DeleteSelection(); override;
        function CanDeleteSelection() : Boolean; override;
        procedure DeleteLine(); override;
        procedure SelectAll(); override;

        {由于TRichEdit提供了FindNext放法，所以现在的TssnRichEditor的FindNext的代码实现就更简单了
          其他的函数的实现和TMemo基本一样
          }
        function FindNext(Text : String; Option : TFindOptions) : Boolean; override;
        function Replace(FindText, ReplaceText : String; Option : TFindOptions) : Integer; override;
        // GetWordCount
        function GetWordWrap() : Boolean; override;
        procedure SetWordWrap(WordWrap : Boolean); override;
    end;

implementation

{ TssnRichEditor }

function TssnRichEditor.CanCopy: Boolean;
begin
    Result := m_Edit.SelLength <> 0;
end;

function TssnRichEditor.CanCut: Boolean;
begin
    Result := m_Edit.SelLength <> 0;
end;

function TssnRichEditor.CanDeleteSelection: Boolean;
begin
    Result := m_Edit.SelLength <> 0;
end;

function TssnRichEditor.CanPaste: Boolean;
begin
    Result := true;
end;

function TssnRichEditor.CanRedo: Boolean;
begin
    Result := m_Edit.CanUndo;
end;

function TssnRichEditor.CanUndo: Boolean;
begin
    Result := m_Edit.CanUndo;
end;

procedure TssnRichEditor.Copy;
begin
    m_Edit.CopyToClipboard();
end;

constructor TssnRichEditor.Create(ParentCtrl: TWinControl);
begin
    m_Edit := TRichEdit.Create(nil);
    m_Edit.PlainText := true;
    m_Edit.Parent := ParentCtrl;
    m_Edit.Align := alClient;
    m_Edit.Visible := true;
    m_Edit.WordWrap := false;
    m_Edit.ScrollBars := ssBoth;
    m_Edit.OnMouseUp := OnMouseUp;
    m_Edit.OnKeyUp := OnKeyUp;
    if m_Edit.CanFocus() then
        m_Edit.SetFocus();
end;

procedure TssnRichEditor.Cut;
begin
    m_Edit.CutToClipboard();
end;

procedure TssnRichEditor.DeleteLine;
begin
    m_Edit.SelStart := m_Edit.SelStart - m_Edit.CaretPos.X;
    m_Edit.SelLength := Length(m_Edit.Lines[m_Edit.CaretPos.Y]) + 2;
    m_Edit.ClearSelection();
end;

procedure TssnRichEditor.DeleteSelection;
begin
    m_Edit.ClearSelection();
end;

destructor TssnRichEditor.Destroy;
begin
    m_Edit.Free();
    m_Edit := nil;
end;

procedure TssnRichEditor.DoLoadFromFile(FileName: String);
begin
    m_Edit.Lines.LoadFromFile(FileName);
end;

function TssnRichEditor.FindNext(Text: String;
  Option: TFindOptions): Boolean;
var
    FoundAt : LongInt;
    StartPos, ToEnd : Integer;
begin
    Result := false;

    StartPos := m_Edit.SelStart + m_Edit.SelLength; //查找起始位置
    ToEnd := Length(m_Edit.Text) - StartPos;        //查找终止位置

    //具体的查找由TRichEdit.FindNext实现
    if (frMatchCase in Option) then
       FoundAt := m_Edit.FindText(Text, StartPos, ToEnd, [stMatchCase])
    else
       FoundAt := m_Edit.FindText(Text, StartPos, ToEnd, []);

    if FoundAt = -1 then    //如果没找到
        Exit;

    m_Edit.SetFocus();
    m_Edit.SelStart := FoundAt;
    m_Edit.SelLength := Length(WideString(Text));
    Result := true;
end;

function TssnRichEditor.GetSaved: Boolean;
begin
    Result := not m_Edit.Modified;
end;

function TssnRichEditor.GetSelectText: String;
begin
    Result := m_Edit.SelText;
end;

function TssnRichEditor.GetText: String;
begin
    Result := m_Edit.Text;
end;

function TssnRichEditor.GetWordWrap: Boolean;
begin
    Result := m_Edit.WordWrap;
end;

procedure TssnRichEditor.OnKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    OnEditorSelectionChange(Sender);
end;

procedure TssnRichEditor.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    OnEditorSelectionChange(Sender);
end;

procedure TssnRichEditor.Paste;
begin
    m_Edit.PasteFromClipboard();
end;

procedure TssnRichEditor.Redo;
begin
    m_Edit.Undo();
end;

function TssnRichEditor.Replace(FindText, ReplaceText: String;
  Option: TFindOptions): Integer;
var
    SelText : String;
begin
    Result := 0;
    
    if frMatchCase in Option then
    begin
        SelText := UpperCase(m_Edit.SelText);
        FindText := UpperCase(FindText);
    end
    else
        SelText := m_Edit.SelText;

    if FindText = SelText then
    begin
        m_Edit.SelText := ReplaceText;
        Result := 1;
    end;

    if not (frReplaceAll in Option) then
        Exit;

    while FindNext(FindText, Option) do
    begin
        m_Edit.SelText := ReplaceText;
        Inc(Result);
    end;
end;

procedure TssnRichEditor.SaveToFile(FileName: String);
begin
    m_Edit.Lines.SaveToFile(FileName);
end;

procedure TssnRichEditor.SelectAll;
begin
    m_Edit.SelectAll();
end;

procedure TssnRichEditor.SetFont(Font: TFont);
begin
    m_Edit.Font := Font;
end;

procedure TssnRichEditor.SetWordWrap(WordWrap: Boolean);
begin
    m_Edit.WordWrap := WordWrap;
end;

procedure TssnRichEditor.Undo;
begin
    m_Edit.Undo();
end;

end.
