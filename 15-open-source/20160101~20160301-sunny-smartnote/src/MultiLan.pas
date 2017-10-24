////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   MultiLan.pas
//  Creator     :   Shen Min
//  Date        :   2002-4-3
//  Comment     :   MultiLanguage support
//
//
////////////////////////////////////////////////////////////////////////////////

unit MultiLan;

{MultiLan.pas 单元是为实现多语言版本而准备的。
  在该单元中， 定义了所有程序中使用到的多种语言的字符串资源以及一套可以动态指向不同字符串资源的字符指针。
  在编译时，使用条件编译符决定编译的语言版本，那套字符指针便被初始化为指向所对应的语言的字符串资源
  }

interface

Type
    LanguageType = (LT_English, LT_Chinese);

const
    English_NoTitle         = 'No Tilte';
    Chinese_NoTitle         = '无标题';

    English_PromptSave      = 'Save changes?';
    Chinese_PromptSave      = '该文件被修改过，是否保存？';

    English_SaveError       = 'Save file failed, maybe it''s read only';
    Chinese_SaveError       = '保存文件失败，可能由于该文件为只读文件';

    English_LoadError       = 'The file can''t be opened : %s';
    Chinese_LoadError       = '打开 %s 文件失败';

    English_NotFindText     = 'Sorry, can''t find string "%s"';
    Chinese_NotFindText     = '抱歉，没有找到 “%s”';

    English_ReplacedAll     = '%d occurrences have been replaced';
    Chinese_ReplacedAll     = '%d 处已被替换';

    English_CountResult     = 'WordCount Result: ';
    Chinese_CountResult     = '字数统计结果：';

    English_AnsiChar        = 'Ansi character: ';
    Chinese_AnsiChar        = '英文字符：';

    English_MultiChar       = 'Multibyte character: ';
    Chinese_MultiChar       = '中文字符：';

    English_NumChar         = 'Numeric character: ';
    Chinese_NumChar         = '数字字符：';

    English_OtherChar       = 'Other character: ';
    Chinese_OtherChar       = '其它字符：';

var
    str_NoTitle,
    str_PromptSave,
    str_SaveError,
    str_LoadError,
    str_NotFindText,
    str_ReplacedAll,
    str_CountResult,
    str_AnsiChar,
    str_MultiChar,
    str_NumChar,
    str_OtherChar : PChar;

implementation

procedure InitLanguage(Language : LanguageType);
begin
    if Language = LT_Chinese then
    begin
        str_NoTitle         := Chinese_NoTitle;
        str_PromptSave      := Chinese_PromptSave;
        str_SaveError       := Chinese_SaveError;
        str_LoadError       := Chinese_LoadError;
        str_NotFindText     := Chinese_NotFindText;
        str_ReplacedAll     := Chinese_ReplacedAll;
        str_CountResult     := Chinese_CountResult;
        str_AnsiChar        := Chinese_AnsiChar;
        str_MultiChar       := Chinese_MultiChar;
        str_NumChar         := Chinese_NumChar;
        str_OtherChar       := Chinese_OtherChar;
    end
    else // default is English
    begin
        str_NoTitle         := English_NoTitle;
        str_PromptSave      := English_PromptSave;
        str_SaveError       := English_SaveError;
        str_LoadError       := English_LoadError;
        str_NotFindText     := English_NotFindText;
        str_ReplacedAll     := English_ReplacedAll;
        str_CountResult     := English_CountResult;
        str_AnsiChar        := English_AnsiChar;
        str_MultiChar       := English_MultiChar;
        str_NumChar         := English_NumChar;
        str_OtherChar       := English_OtherChar;
    end;
end;


initialization
    {$IFDEF _CHS}
    InitLanguage(LT_Chinese);
    {$ELSE}
    InitLanguage(LT_English);
    {$ENDIF}


end.
