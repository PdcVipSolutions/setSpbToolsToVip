﻿% Copyright (c) Prolog Development Center SPb

implement main
    open core
    open xmlNavigate

clauses
    run() :-
        CP = commandLineParser::new(),
        CP:acceptEmpty := true,
        CP:addOption_help("-help"),
        % define command line options here
        if ErrorMessage = isSome(CP:parse()) then
            stdio::write(ErrorMessage)
        else
            % place your own code here
            setSpbToolsToVip()
        end if,
        _ = console::readLine().

class facts - vip_Variants
    variant_F : (registry::rootKey, string VipPlace, string VipKey).

constants
    vipVariants_FileName_C = @"..\AppData\vip_Variants.par".

class predicates
    setSpbToolsToVip : ().
clauses
    setSpbToolsToVip() :-
        CurrentDir = directory::getCurrentDirectory(),
        if file::existExactFile(vipVariants_FileName_C) then
            file::consult(vipVariants_FileName_C, vip_Variants),
            VariantListSrc =
                [ VipPath ||
                    variant_F(RootKey, VipPlace, VipKey),
                    try
                        stdio::writef("try [%: %]\n", RootKey, VipPlace),
                        string(VipPath) = registry::getValue(RootKey, VipPlace, VipKey, registry::key_wow64_32key),
                        stdio::writef("succeeded VipPath-> [%]\n", VipPath)
                    catch _Err do
                        stdio::writef("excepted [%: %]\n", RootKey, VipPlace),
                        fail
                    end try
                ],
            VariantList = list::removeDuplicates(VariantListSrc),
            chooseAndPerformVipVariant(VariantList)
        else
            stdio::writef("No file [%] found.\nNo settings may be done.\n", filename::createPath(CurrentDir, vipVariants_FileName_C))
        end if,
        stdio::write("Press any key to leave\n").

class facts
    counter_V : integer := 1.
    vipDir_V : string := erroneous.
    currentDir_V : string := directory::getCurrentDirectory().

class predicates
    chooseAndPerformVipVariant : (string* VariantList).
clauses
    chooseAndPerformVipVariant([]) :-
        !,
        stdio::write("No Vip installed found\n").
    chooseAndPerformVipVariant([Variant]) :-
        !,
        vipDir_V := Variant,
        setToolsToVip(),
        stdio::writef("Complete for placement [%]\n", Variant).
    chooseAndPerformVipVariant(VariantList) :-
        NoOfElements = list::length(VariantList),
        stdio::writef("Several =[%]= Vip placements found:\n", NoOfElements),
        foreach Variant in VariantList do
            stdio::writef("\t%: %\n", counter_V, Variant),
            counter_V := counter_V + 1
        end foreach,
        stdio::write("Choose Variant No to be used: "),
        hasDomain(string, Response),
        Response = console::readLine(),
        try
            ResponsePos = toTerm(Response),
            if ResponsePos >= 1 and ResponsePos < NoOfElements + 1 then
                Variant = list::nth(ResponsePos - 1, VariantList),
                chooseAndPerformVipVariant([Variant])
            else
                stdio::writef("Response [%] is not in in the range [1 .. %]\n", Response, NoOfElements)
            end if,
            stdio::writef("Complete\n")
        catch Trace do
            if Description = exception::tryGetExceptionDescription(Trace) then
                stdio::write(Description, "\n")
            else
                stdio::write("Unsuccessfull performing\n")
            end if
        end try.

class predicates
    setToolsToVip : ().
clauses
    setToolsToVip() :-
        setIdeVariables(),
        setProjectTemplates(),
        setSourceTemplates(),
        setPzlRule(),
        setWSM_Options().

class facts - spb_idevars
% IDE Variable
    idevar_F : (string VarName, string VarRelativePath).

class facts - vip_idevars
    td : (string VarName, string VarFullPath).

constants
    vipIdeVars_FileName_C = @"AppData\ide.vars".
    spbIdeVars_FileName_C = @"..\AppData\ide.vars".
    wsm_VipIdeVars_FileName_C = @"AppData\wsm_ide.vars".

class predicates
    setIdeVariables : ().
clauses
    setIdeVariables() :-
        stdio::write("Creating\\adding IDE variables to file ide.vars in Vip\n"),
        VipIdeVarsFile = fileName::createPath(vipDir_V, vipIdeVars_FileName_C),
        if file::existExactFile(VipIdeVarsFile) then
            file::consult(VipIdeVarsFile, vip_idevars),
            Actual_VipIdeVarsFile = VipIdeVarsFile
        else
            Actual_VipIdeVarsFile = fileName::createPath(vipDir_V, wsm_VipIdeVars_FileName_C),
            stdio::write("File ide.vars in Vip not found\nWill be used file wsm_ide.vars")
        end if,
        if file::existExactFile(spbIdeVars_FileName_C) then
            file::consult(spbIdeVars_FileName_C, spb_idevars),
            foreach idevar_F(VarName, _VarRelativePath) do
                retractall(td(VarName, _))
            end foreach,
            retractall(td("VipDir", _)),
            !,
            foreach idevar_F(DirName, VarRelativePath) do
                Path = fileName::createPath(currentDir_V, VarRelativePath),
                assert(td(DirName, Path))
            end foreach,
            assert(td("VipDir", vipDir_V)),
            file::save(Actual_VipIdeVarsFile, vip_idevars),
            Key = @"Software\Prolog Development Center\Visual Prolog6\settings\toolsDirList\",
            registry::setValue(registry::currentUser(), Key,
                [ namedValue(Var, string(ToolPath)) ||
                    idevar_F(Var, SrcPath),
                    ToolPath = fileName::createPath(currentDir_V, SrcPath)
                ]),
            stdio::write("Done\n")
        else
            stdio::writef("File [%] not found.\nNo New IDE variables added/created.\n", spbIdeVars_FileName_C)
        end if.

constants
    spbPzlRule_FileName_C = @"..\AppData\PZL.rules".

class predicates
    setPzlRule : ().
clauses
    setPzlRule() :-
        stdio::write("Copying Pzl Rules file pzl.rules\n"),
        if file::existExactFile(spbPzlRule_FileName_C) then
            file::copy(spbPzlRule_FileName_C, fileName::createPath(vipDir_V, @"AppData\_Rules\PZL.rules")),
            stdio::write("Done\n")
        else
            stdio::writef("File [%] in spbVipTools not found\n", spbPzlRule_FileName_C)
        end if.

class facts - vip_projects
% Project
    pattern : (string TemplateTitle, string TemplatePath, string Comment).

class facts - spb_projects
    spbPrj_pattern_F : (string TemplateTitle, string TemplatePath, string Comment).

constants
    spbProjectTemplatesDir_C = @"..\AppData\ProjectTemplates\".
    vipProjectTemplatesDir_C = @"AppData\ProjectTemplates\".

class predicates
    setProjectTemplates : ().
clauses
    setProjectTemplates() :-
        stdio::write("Copying ProjectTemplates to Vip\\appData\n"),
        VipProjectTemplatesDir = fileName::createPath(vipDir_V, vipProjectTemplatesDir_C),
        if directory::existExactDirectory(fileName::createPath(currentDir_V, spbProjectTemplatesDir_C))
            and directory::existExactDirectory(fileName::createPath(vipDir_V, VipProjectTemplatesDir))
        then
            directory::copyDirectory(spbProjectTemplatesDir_C, string::concat(VipProjectTemplatesDir, "SpbTemplates"), true),
            stdio::write("Updating Project Templates registry at VipDir\\appData\\ProjectTemplates\n"),
            file::consult(fileName::createPath(vipDir_V, string::concat(vipProjectTemplatesDir_C, "_order.dba")), vip_projects),
            file::consult(@"..\AppData\project_templates.par", spb_projects),
            foreach spbPrj_pattern_F(TemplateTitle, TemplatePath, Comment) do
                retractAll(pattern(TemplateTitle, _, _)),
                assert(pattern(TemplateTitle, TemplatePath, Comment))
            end foreach,
            file::save(fileName::createPath(VipProjectTemplatesDir, "_order.dba"), vip_projects),
            stdio::write("Done\n"),
            succeed()
        else
            stdio::write("ProjectTemplates directory in spbVipTools not found\n")
        end if.

class facts - vip_classes
% Class:: Name, Folder, Description, Module to add, Module to open
    pattern : (string TemplateTitle, string TemplatePath, string Comment, string ModuleToAdd, string ModuleToOpen).

class facts - spb_classes
    spb_pattern_F : (string TemplateTitle, string TemplatePath, string Comment, string ModuleToAdd, string ModuleToOpen).

constants
    spbSourceTemplatesDir_C = @"..\AppData\SourceTemplates\".
    vipSourceTemplatesDir_C = @"AppData\SourceTemplates\".

class predicates
    setSourceTemplates : ().
clauses
    setSourceTemplates() :-
        stdio::write("Copying ClassTemplates to Vip\\appData\n"),
        SpbTemplatesDir = fileName::createPath(vipDir_V, string::concat(vipSourceTemplatesDir_C, "SpbTemplates")),
        VipTemplatesList = fileName::createPath(vipDir_V, string::concat(vipSourceTemplatesDir_C, "_order.dba")),
        SpbTemplatesList = @"..\AppData\spb_classes.par",
        directory::copyDirectory(spbSourceTemplatesDir_C, SpbTemplatesDir, true),
        stdio::write("Classes copying Done\n"),
        %
        stdio::write("Updating Class Templates registry at Vip\\appData\\SourceTemplates\n"),
        file::consult(VipTemplatesList, vip_classes),
        file::consult(SpbTemplatesList, spb_classes),
        foreach spb_pattern_F(TemplateTitle, TemplatePath, Comment, ModuleToAdd, ModuleToOpen) do
            retractall(pattern(TemplateTitle, _, _, _, _)),
            assert(pattern(TemplateTitle, TemplatePath, Comment, ModuleToAdd, ModuleToOpen))
        end foreach,
        file::save(VipTemplatesList, vip_classes),
        stdio::write("Done\n"),
        %
        succeed().

class facts
    wsmOptions_V : xmlDocument := erroneous.

class predicates
    setWSM_Options : ().
clauses
    setWSM_Options() :-
        wsmOptions_V := xmlDocument::new("wsm_options"),
        wsmOptions_V:codePage_P := utf8,
        wsmOptions_V:indent_P := true,
        wsmOptions_V:xmlStandalone_P := xmlLite::yes,
        OptionsFile = fileName::createPath(currentDir_V, @"wsmAppData\OptionsWSM.xml"),
        XmlOptions = inputStream_file::openFile(OptionsFile, stream::binary),
        spbXmlLigntSupport::read(XmlOptions, wsmOptions_V),
        XmlOptions:close(),
        if VirtualDirNode =
                wsmOptions_V:getNode_nd([root(), child("be_options", { (_) }), child("group", { (O) :- O:attribute("title") = "VirtualDir" })])
            and !
        then
            if DirVar = VirtualDirNode:getNode_nd([child("VirtDir", { (O) :- O:attribute("VirtName") = "$(VipDir)" })]) and ! then
                if _VipDir = DirVar:attribute("path") then
                    DirVar:modifyAttribute("path", vipDir_V)
                else
                    DirVar:addAttribute("path", vipDir_V)
                end if
            else
                DirVar = xmlElement::new("VirtDir", VirtualDirNode),
                DirVar:addAttribute("VirtName", "$(VipDir)"),
                DirVar:addAttribute("path", vipDir_V),
                VirtualDirNode:addNode(DirVar)
            end if
        end if,
        if Fe_Options =
                wsmOptions_V:getNode_nd([root(), child("fe_options", { (_) }), child("group", { (O) :- O:attribute("title") = "wsFE_Tasks" })])
            and !
        then
            updateAttribute(Fe_Options, "work_space", "path", fileName::createPath(currentDir_V, @"..\UserWS\")),
            updateAttribute(Fe_Options, "source", "path", currentDir_V),
            updateAttribute(Fe_Options, "folder", "path", currentDir_V),
            updateAttribute(Fe_Options, "filename", "path", fileName::createPath(currentDir_V, @"..\UserWS\DemoWorkSpace.wsm")),
            succeed()
        else
            stdio::writef("The node [%.%] with the title [%] not found\n", "be_options", "group", "wsFE_Tasks")
        end if,
        OutputStream = outputStream_file::create(OptionsFile, stream::binary),
        try
            wsmOptions_V:saveXml(OutputStream)
        catch TraceID do
            foreach Descriptor = exception::getDescriptor_nd(TraceID) do
                stdio::writef("Error [%]\n", Descriptor)
            end foreach
        end try.

class predicates
    updateAttribute : (xmlElement NodeObj, string SubNodeName, string AttributeName, string AttrValue).
clauses
    updateAttribute(NodeObj, SubNodeName, AttributeName, AttrValue) :-
        if SubNodeObj = NodeObj:getNode_nd([child(SubNodeName, { (_) })]) and ! then
            if _VipDir = SubNodeObj:attribute(AttributeName) then
                SubNodeObj:modifyAttribute(AttributeName, AttrValue)
            else
                SubNodeObj:addAttribute(AttributeName, AttrValue)
            end if
        else
            SubNodeObj = xmlElement::new(SubNodeName, NodeObj),
            SubNodeObj:addAttribute(AttributeName, AttrValue),
            NodeObj:addNode(SubNodeObj)
        end if.

end implement main

goal
    console::runUtf8(main::run).
