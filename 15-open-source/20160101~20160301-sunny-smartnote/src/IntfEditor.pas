////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   IntfEditor.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-13
//  Comment     :   Interface of Editor
//
//
////////////////////////////////////////////////////////////////////////////////

unit IntfEditor;

interface

uses Graphics, Classes, Dialogs;

type
    TssnWordCountRec = record
        AnsiChar : Integer;
        MultiChar : Integer;
        NumChar : Integer;
        Other : Integer;
    end;

    {TssnWorkSpace代理了TssnEditor的功能，为了保证发送给TssnWorkSapce的与编辑器组件有关的请求都能够正确地转送给TssnEditor处理，
      它们必须拥有一个相同的功能子集，所以两者继承一个共同的基类IssnEditor

      IssnEditor是一个接口（抽象类），它规定了它的派生类必须实现哪些功能。当TssnWorkSpace和TssnEditor都从它派生时，TssnWorkSpace
        就会有绝对的“自信”将它接到的请求转发给其内部的TssnEditor实例
      所以IssnEditor只需要定义接口，不需要实现
      }
    IssnEditor = class
    public
        function GetFileName() : String; virtual; abstract;        //获取工作区所对应的文件名
        function GetSaved() : Boolean; virtual; abstract;          //获取工作区是否处于“已保存”状态
        function Save() : Boolean; virtual; abstract;              //以当前文件名保存工作区内的文本到文件
        function SaveAs() : Boolean; virtual; abstract;            //另存为
        function GetSelectText() : String; virtual; abstract;      //获取工作区正被选中的字符串
        procedure SetFont(Font : TFont); virtual; abstract;        //设置工作区显示字体
        procedure Undo(); virtual; abstract;                       //执行“撤销”操作
        function CanUndo() : Boolean; virtual; abstract;           //工作区是否处于允许执行“撤销”操作状态
        procedure Redo(); virtual; abstract;                       //执行“重做”操作
        function CanRedo() : Boolean; virtual; abstract;           //工作区是否处于允许执行“重做”操作状态
        procedure Cut(); virtual; abstract;                        //执行“剪切”操作
        function CanCut() : Boolean; virtual; abstract;            //工作区是否处于允许执行“剪切”操作状态
        procedure Copy(); virtual; abstract;                       //执行“复制“操作
        function CanCopy() : Boolean; virtual; abstract;           //工作区是否处于允许执行“复制”操作状态
        procedure Paste(); virtual; abstract;                      //执行”粘贴“操作
        function CanPaste() : Boolean; virtual; abstract;          //工作区是否处于允许执行“粘贴”操作状态
        procedure DeleteSelection(); virtual; abstract;            //执行”删除所选字符“操作
        function CanDeleteSelection() : Boolean; virtual; abstract;//工作区是否处于允许执行“删除所选字符”操作状态
        procedure DeleteLine(); virtual; abstract;                 //执行”删除一行“操作
        procedure SelectAll(); virtual; abstract;                  //执行”全选“操作
        function FindNext(Text : String; Option : TFindOptions) : Boolean; virtual; abstract;                //查找下一个
        function Replace(FindText, ReplaceText : String; Option : TFindOptions) : Integer; virtual; abstract;//替换
        function GetWordCount() : TssnWordCountRec; virtual; abstract;  //统计当前工作区字数
        function GetWordWrap() : Boolean; virtual; abstract;            //获取当前工作区是否处于“自动换行”状态
        procedure SetWordWrap(WordWrap : Boolean); virtual; abstract;   //设置”自动换行“
    end;

implementation

end.
