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
    {随着TssnEditor的实现完成，现在可能急于想实现一个具体的编辑器组件已让它工作起来，也可以暂时满足一下我们的成就感
      TssnMemoEditor就是使用TMemo来实现的一个比较简单的编辑器组件类
        它将实现IssnEditor接口中TssnEditor未实现的部分，以及TssnEditor声明的抽象虚方法
    TssnMemoEditor依赖于TMemo，因此它本身的实现并不复杂，很多都是直接调用TMemo的方法实现的，这也算是重用带来的好处吧
      假如以后的某一天找到了更好的编辑器组件，那么只需要重新实现一个类似的TssnMemoEditor的类
      所以可扩展性还是很好
      }
    TssnMemoEditor = class(TssnEditor)
    private
        {内部包含一个TMemo实例，TssnMemoEditor只是对它进行接口改造
          TssnMemoEditor的实现全部依赖于这个TMemo
          TssnMemoEditor其实就是将TMemo的接口转换成TssnEditor的接口的“接口转换机”
          }
        m_Edit : TMemo;

    protected
        {DoLoadFromFile、SaveToFile、GetText方法（override）
          在介绍TssnEditor的时候提到过，编辑器读取文本的接口DoLoadFromFile、SaveToFile这两个方法。
            DoLoadFromFile是在TssnEditor中声明的抽象虚方法，延迟到TssnEditor的派生类实现
            同样，保存文件也是Save、SaveAs和SaveToFile，其中SaveToFile为抽象虚方法，延迟到TssnEditor的派生类实现
          还有配合统计字数功能实现的GetText方法，延迟到TssnEditor的派生类来实现
          }
        procedure DoLoadFromFile(FileName : String); override;    //TssnEditor中声明的抽象虚方法，在TssnMemoEditor中需要实现这个方法

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
        function FindNext(Text : String; Option : TFindOptions) : Boolean; override;                 //查找下一个
        function Replace(FindText, ReplaceText : String; Option : TFindOptions) : Integer; override; //替换
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

{ParentCtrl参数为该TMemo组件实例所在容器
  具体可以同时参考调用Create方法的代码部分
  }
constructor TssnMemoEditor.Create(ParentCtrl: TWinControl);
begin
    m_Edit := TMemo.Create(nil);
    m_Edit.Parent := ParentCtrl;      //指定容器
    m_Edit.Align := alClient;
    m_Edit.Visible := true;
    m_Edit.WordWrap := false;
    m_Edit.ScrollBars := ssBoth;

    {设置m_Edit的事件处理函数指针
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

{FindNext的算法是：首先按照查找方向（向上或者向下）取出查找范围内所有的字符串
  然后，如果是非大小写敏感查找，则将字符串中所有字符转换成大写字符
  最后根据查找方向搜索要查找的字符串
  如果找到，则将光标定位到找到的字符串并返回True，否则返回False
  
TFindOption在Delphi自带的Dialogs单元定义
  TFindOption = (frDown, frFindNext, frHideMatchCase, frHideWholeWord,
    frHideUpDown, frMatchCase, frDisableMatchCase, frDisableUpDown,
    frDisableWholeWord, frReplace, frReplaceAll, frWholeWord, frShowHelp);
  TFindOptions = set of TFindOption;
第一个参数是：要查找的字符串
第二个参数是：查找的属性
  }
function TssnMemoEditor.FindNext(Text: String; Option: TFindOptions) : Boolean;
var
    FoundAt : Integer;
    LastFoundAt : Integer;
    AllText : String;
begin
    Result := false;

    {TMemo的SelStart属性：SelStart是开始选择的起始位置
      Tmemo的SelLength属性：从指定的起始位置SelStart开始选择的长度值
      System.Copy函数：
        str := “123456”;str1 := Copy(Str,2,3);
        结果是 str1 等于234。
        Copy有3个参数，第一个是要处理的字符串，第二个是要截取的开始位置，第三个是截取位数。
        当第三个参数大于字符长度，那么效果就是取开始位置 后的所有字符。str1 := Copy(Str,2,10); 结果就是str1 等于 23456。 
      }
    if frDown in Option then         //向下查找，找出搜索范围
        AllText := System.Copy(m_Edit.Text, m_Edit.SelStart + m_Edit.SelLength + 1, Length(m_Edit.Text))   //拷贝从当前选取的字符串结尾到整个编辑器文本结束的字符串
    else
        AllText := System.Copy(m_Edit.Text, 1, m_Edit.SelStart);    //拷贝从编辑器文本开始到当期选取的字符串开始的字符串

    if frMatchCase in Option then   //大小写不敏感
    begin
        AllText := UpperCase(AllText);
        Text := UpperCase(Text);
    end;

    if frDown in Option then      //执行“向下查找”
    begin
        {pos函数功能：取出子串在父串中第一次出现的位置
          例如：pos('b','abcd');    返回结果是2。
          }
        FoundAt := Pos(Text, AllText);
        if FoundAt = 0 then        //如果没有找到
            Exit;
        m_Edit.SelStart := m_Edit.SelStart + m_Edit.SelLength + FoundAt;        //这就是定位光标到执行字符串的代码逻辑
    end
    else    //执行“向上查找”
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
        m_Edit.SelStart := LastFoundAt;     //定位光标到找到的字符串
    end;

    m_Edit.SelLength := Length(Text);       //选中找到的字符串
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
    {读取文本到编辑器}
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
    //直接调用TMemo的Undo方法
    //很多的编辑操作其实就是直接简单的调用TMemo的接口
    m_edit.Undo();
end;

{替换算法是：首先检查当前选中的是否为要被替换的字符串
  如果是则替换
  然后判断是不是“替换全部”，是则循环调用FindNext查找到被替换字符串然后替换，直至循环退出（找不到下一个时），否则退出
参数解释：
  FindText：要被替换的字符串
  ReplaceText：要替换成的字符串
  }
function TssnMemoEditor.Replace(FindText, ReplaceText: String;
  Option: TFindOptions): Integer;
var
    SelText : String;
begin
    Result := 0;
    
    if frMatchCase in Option then      //大小写敏感
    begin
        SelText := UpperCase(m_Edit.SelText);
        FindText := UpperCase(FindText);
    end
    else
        SelText := m_Edit.SelText;

    //若当前选择字符串与要替换的字符串相同，则替换    
    if FindText = SelText then
    begin
        m_Edit.SelText := ReplaceText;
        Result := 1;
    end;

    if not (frReplaceAll in Option) then    //如果并非“替换所有”，则结束
        Exit;

    //查找下一个，并替换
    while FindNext(FindText, Option) do
    begin
        m_Edit.SelText := ReplaceText;
        Inc(Result);
    end;
end;

procedure TssnMemoEditor.SaveToFile(FileName: String);
begin
    {真正完成保存文件的动作}
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
    {获取编辑器中所有文本}
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
    {在析构方法中销毁m_Edit
      }
    m_Edit.Free();
    m_Edit := nil;
end;

end.
