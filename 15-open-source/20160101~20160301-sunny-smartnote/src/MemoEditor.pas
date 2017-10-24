////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   MemoEditor.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-2
//  Comment     :
//
//
////////////////////////////////////////////////////////////////////////////////

unit MemoEditor;

interface

uses StdCtrls, Controls, Graphics, Classes, Dialogs, SysUtils,
     Editor, IntfEditor;

type
    {����TssnEditor��ʵ����ɣ����ڿ��ܼ�����ʵ��һ������ı༭���������������������Ҳ������ʱ����һ�����ǵĳɾ͸�
      TssnMemoEditor����ʹ��TMemo��ʵ�ֵ�һ���Ƚϼ򵥵ı༭�������
        ����ʵ��IssnEditor�ӿ���TssnEditorδʵ�ֵĲ��֣��Լ�TssnEditor�����ĳ����鷽��
    TssnMemoEditor������TMemo������������ʵ�ֲ������ӣ��ܶ඼��ֱ�ӵ���TMemo�ķ���ʵ�ֵģ���Ҳ�������ô����ĺô���
      �����Ժ��ĳһ���ҵ��˸��õı༭���������ôֻ��Ҫ����ʵ��һ�����Ƶ�TssnMemoEditor����
      ���Կ���չ�Ի��Ǻܺ�
      }
    TssnMemoEditor = class(TssnEditor)
    private
        {�ڲ�����һ��TMemoʵ����TssnMemoEditorֻ�Ƕ������нӿڸ���
          TssnMemoEditor��ʵ��ȫ�����������TMemo
          TssnMemoEditor��ʵ���ǽ�TMemo�Ľӿ�ת����TssnEditor�Ľӿڵġ��ӿ�ת������
          }
        m_Edit : TMemo;

    protected
        {DoLoadFromFile��SaveToFile��GetText������override��
          �ڽ���TssnEditor��ʱ���ᵽ�����༭����ȡ�ı��Ľӿ�DoLoadFromFile��SaveToFile������������
            DoLoadFromFile����TssnEditor�������ĳ����鷽�����ӳٵ�TssnEditor��������ʵ��
            ͬ���������ļ�Ҳ��Save��SaveAs��SaveToFile������SaveToFileΪ�����鷽�����ӳٵ�TssnEditor��������ʵ��
          �������ͳ����������ʵ�ֵ�GetText�������ӳٵ�TssnEditor����������ʵ��
          }
        procedure DoLoadFromFile(FileName : String); override;    //TssnEditor�������ĳ����鷽������TssnMemoEditor����Ҫʵ���������

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
        function FindNext(Text : String; Option : TFindOptions) : Boolean; override;                 //������һ��
        function Replace(FindText, ReplaceText : String; Option : TFindOptions) : Integer; override; //�滻
        // GetWordCount
        function GetWordWrap() : Boolean; override;
        procedure SetWordWrap(WordWrap : Boolean); override;
    end;

implementation

{ TssnMemoEditor }

function TssnMemoEditor.CanCopy: Boolean;
begin
    Result := m_Edit.SelLength <> 0;
end;

function TssnMemoEditor.CanCut: Boolean;
begin
    Result := m_Edit.SelLength <> 0;
end;

function TssnMemoEditor.CanDeleteSelection: Boolean;
begin
    Result := m_Edit.SelLength <> 0;
end;

function TssnMemoEditor.CanPaste: Boolean;
begin
    Result := true;
end;

function TssnMemoEditor.CanRedo: Boolean;
begin
    Result := true;
end;

function TssnMemoEditor.CanUndo: Boolean;
begin
    Result := true;
end;

procedure TssnMemoEditor.Copy;
begin
    m_Edit.CopyToClipboard();
end;

{ParentCtrl����Ϊ��TMemo���ʵ����������
  �������ͬʱ�ο�����Create�����Ĵ��벿��
  }
constructor TssnMemoEditor.Create(ParentCtrl: TWinControl);
begin
    m_Edit := TMemo.Create(nil);
    m_Edit.Parent := ParentCtrl;      //ָ������
    m_Edit.Align := alClient;
    m_Edit.Visible := true;
    m_Edit.WordWrap := false;
    m_Edit.ScrollBars := ssBoth;

    {����m_Edit���¼�������ָ��
      }
    m_Edit.OnMouseUp := OnMouseUp;
    m_Edit.OnKeyUp := OnKeyUp;
    if m_Edit.CanFocus() then
        m_Edit.SetFocus();
end;

procedure TssnMemoEditor.Cut;
begin
    m_Edit.CutToClipboard();
end;

procedure TssnMemoEditor.DeleteLine;
begin
    m_Edit.SelStart := m_Edit.SelStart - m_Edit.CaretPos.X;
    m_Edit.SelLength := Length(m_Edit.Lines[m_Edit.CaretPos.Y]) + 2;
    m_Edit.ClearSelection();
end;

procedure TssnMemoEditor.DeleteSelection;
begin
    m_Edit.ClearSelection();
end;

{FindNext���㷨�ǣ����Ȱ��ղ��ҷ������ϻ������£�ȡ�����ҷ�Χ�����е��ַ���
  Ȼ������ǷǴ�Сд���в��ң����ַ����������ַ�ת���ɴ�д�ַ�
  �����ݲ��ҷ�������Ҫ���ҵ��ַ���
  ����ҵ����򽫹�궨λ���ҵ����ַ���������True�����򷵻�False
  
TFindOption��Delphi�Դ���Dialogs��Ԫ����
  TFindOption = (frDown, frFindNext, frHideMatchCase, frHideWholeWord,
    frHideUpDown, frMatchCase, frDisableMatchCase, frDisableUpDown,
    frDisableWholeWord, frReplace, frReplaceAll, frWholeWord, frShowHelp);
  TFindOptions = set of TFindOption;
��һ�������ǣ�Ҫ���ҵ��ַ���
�ڶ��������ǣ����ҵ�����
  }
function TssnMemoEditor.FindNext(Text: String; Option: TFindOptions) : Boolean;
var
    FoundAt : Integer;
    LastFoundAt : Integer;
    AllText : String;
begin
    Result := false;

    {TMemo��SelStart���ԣ�SelStart�ǿ�ʼѡ�����ʼλ��
      Tmemo��SelLength���ԣ���ָ������ʼλ��SelStart��ʼѡ��ĳ���ֵ
      System.Copy������
        str := ��123456��;str1 := Copy(Str,2,3);
        ����� str1 ����234��
        Copy��3����������һ����Ҫ������ַ������ڶ�����Ҫ��ȡ�Ŀ�ʼλ�ã��������ǽ�ȡλ����
        �����������������ַ����ȣ���ôЧ������ȡ��ʼλ�� ��������ַ���str1 := Copy(Str,2,10); �������str1 ���� 23456�� 
      }
    if frDown in Option then         //���²��ң��ҳ�������Χ
        AllText := System.Copy(m_Edit.Text, m_Edit.SelStart + m_Edit.SelLength + 1, Length(m_Edit.Text))   //�����ӵ�ǰѡȡ���ַ�����β�������༭���ı��������ַ���
    else
        AllText := System.Copy(m_Edit.Text, 1, m_Edit.SelStart);    //�����ӱ༭���ı���ʼ������ѡȡ���ַ�����ʼ���ַ���

    if frMatchCase in Option then   //��Сд������
    begin
        AllText := UpperCase(AllText);
        Text := UpperCase(Text);
    end;

    if frDown in Option then      //ִ�С����²��ҡ�
    begin
        {pos�������ܣ�ȡ���Ӵ��ڸ����е�һ�γ��ֵ�λ��
          ���磺pos('b','abcd');    ���ؽ����2��
          }
        FoundAt := Pos(Text, AllText);
        if FoundAt = 0 then        //���û���ҵ�
            Exit;
        m_Edit.SelStart := m_Edit.SelStart + m_Edit.SelLength + FoundAt;        //����Ƕ�λ��굽ִ���ַ����Ĵ����߼�
    end
    else    //ִ�С����ϲ��ҡ�
    begin
        LastFoundAt := 0;
        repeat
            FoundAt := Pos(Text, AllText);
            if FoundAt <> 0 then
            begin
                AllText := System.Copy(AllText, FoundAt + 1, Length(AllText));
                LastFoundAt := LastFoundAt + FoundAt;
            end
        until FoundAt = 0;
        if LastFoundAt = 0 then
            Exit;
        m_Edit.SelStart := LastFoundAt;     //��λ��굽�ҵ����ַ���
    end;

    m_Edit.SelLength := Length(Text);       //ѡ���ҵ����ַ���
    Result := true;
    if m_Edit.CanFocus() then
        m_Edit.SetFocus();
end;

function TssnMemoEditor.GetSaved: Boolean;
begin
    Result := not m_Edit.Modified;
end;

function TssnMemoEditor.GetSelectText: String;
begin
    Result := m_Edit.SelText;
end;

procedure TssnMemoEditor.OnKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    OnEditorSelectionChange(Sender);
end;

procedure TssnMemoEditor.DoLoadFromFile(FileName: String);
begin
    {��ȡ�ı����༭��}
    m_Edit.Lines.LoadFromFile(FileName);
end;

procedure TssnMemoEditor.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    OnEditorSelectionChange(Sender);
end;

procedure TssnMemoEditor.Paste;
begin
    m_Edit.PasteFromClipboard();
end;

procedure TssnMemoEditor.Redo;
begin
    //ֱ�ӵ���TMemo��Undo����
    //�ܶ�ı༭������ʵ����ֱ�Ӽ򵥵ĵ���TMemo�Ľӿ�
    m_edit.Undo();
end;

{�滻�㷨�ǣ����ȼ�鵱ǰѡ�е��Ƿ�ΪҪ���滻���ַ���
  ��������滻
  Ȼ���ж��ǲ��ǡ��滻ȫ����������ѭ������FindNext���ҵ����滻�ַ���Ȼ���滻��ֱ��ѭ���˳����Ҳ�����һ��ʱ���������˳�
�������ͣ�
  FindText��Ҫ���滻���ַ���
  ReplaceText��Ҫ�滻�ɵ��ַ���
  }
function TssnMemoEditor.Replace(FindText, ReplaceText: String;
  Option: TFindOptions): Integer;
var
    SelText : String;
begin
    Result := 0;
    
    if frMatchCase in Option then      //��Сд����
    begin
        SelText := UpperCase(m_Edit.SelText);
        FindText := UpperCase(FindText);
    end
    else
        SelText := m_Edit.SelText;

    //����ǰѡ���ַ�����Ҫ�滻���ַ�����ͬ�����滻    
    if FindText = SelText then
    begin
        m_Edit.SelText := ReplaceText;
        Result := 1;
    end;

    if not (frReplaceAll in Option) then    //������ǡ��滻���С��������
        Exit;

    //������һ�������滻
    while FindNext(FindText, Option) do
    begin
        m_Edit.SelText := ReplaceText;
        Inc(Result);
    end;
end;

procedure TssnMemoEditor.SaveToFile(FileName: String);
begin
    {������ɱ����ļ��Ķ���}
    m_Edit.Lines.SaveToFile(FileName);
end;

procedure TssnMemoEditor.SelectAll;
begin
    m_Edit.SelectAll();
end;

procedure TssnMemoEditor.SetFont(Font: TFont);
begin
    m_Edit.Font := Font;
end;

procedure TssnMemoEditor.Undo;
begin
    m_edit.Undo();
end;

function TssnMemoEditor.GetText: String;
begin
    {��ȡ�༭���������ı�}
    Result := m_Edit.Lines.Text;
end;

function TssnMemoEditor.GetWordWrap: Boolean;
begin
    Result := m_Edit.WordWrap;
end;

procedure TssnMemoEditor.SetWordWrap(WordWrap: Boolean);
begin
    m_Edit.WordWrap := WordWrap;
    if WordWrap then
        m_Edit.ScrollBars := ssVertical
    else
        m_Edit.ScrollBars := ssBoth;
end;

destructor TssnMemoEditor.Destroy;
begin
    {����������������m_Edit
      }
    m_Edit.Free();
    m_Edit := nil;
end;

end.
