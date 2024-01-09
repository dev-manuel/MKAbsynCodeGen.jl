module MKAbsynCodeGen

include("main.jl")

using ..MKAbsynProgramTraversal


using MKAbsyn

using ImmutableList
using MetaModelica
using ExportAll

import ListUtil

const INITIAL_SUB_CLASS_NAME = "Context_Initial"



function generateCodeFromMKAbsyn(inProgram::MKAbsyn.Program)::String

    # now context and context equation mappings are full
    # validate contexts and context equation mappings
    return generateCodeFromClassList(inProgram.classes)
end

function generateCodeFromClassList(classList::List{Class})::String
    out = (createModelVariations(class) for class in classList)
    join(out, "\n")
end

function validate(context_equation_dict::Dict{Class,List{ContextEquationSystem}}, context_dict::Dict{Class,List{ContextDeclaration}})::Bool
    #todo: check for duplicate labels in context definition
    #todo: check for duplicate labels in equation sections
    #todo: check for existence of all mentioned contexts in equation sections
    true
end


function createModelVariations(class::MKAbsyn.Class)::String


    @match class.restriction begin
        MKAbsyn.R_CLASS() => begin
            return translateClass(MKAbsynProgramTraverser(), class)
        end
        MKAbsyn.R_MODEL() => begin
            helper = MKAbsynProgramTraverser()
            resetState(helper)

            # getting the contexts (just for traversal - actual translation of programm is ignored)
            translateClass(helper, class)

            # println("=======TRANSLATED PROGRAM==========")
            # println(a)
            # println("===================================")
            context_dict::Dict{Class,List{ContextDeclaration}} = helper.context_dict
            context_equation_dict::Dict{Class,List{ContextEquationSystem}} = helper.context_equation_dict

            if isempty(context_dict)
                println("no context or context equation sections for " + class.name + " -> skipping variationing")
                return translateClass(MKAbsynProgramTraverser(), class)
            end

            if !validate(context_equation_dict, context_dict)
                @error "Contexts not valid"
            end

            return createModelVariation(class, context_equation_dict[class], context_dict[class])
        end
        MKAbsyn.R_PACKAGE() => begin
            return handlePackage(class)
        end
    end


end

function handlePackage(class::Class)::String
    # println(class + "\n\n")
    # return = generateCodeFromClassList(Cons{Class}(class, list()))

    # iterate over class parts
    classDef = class.body
    out = nil
    @match classDef begin
        # PARTS
        MKAbsyn.PARTS(
            #typeVars=typeVars,
            #classAttrs=classAttrs,
            classParts=dClassParts,
            #ann=ann,
            #comment=cmtString,
        ) => begin
            # classes = list()
            classes = list()
            for classPart in dClassParts
                # extract element items from class part
                elementItems = @match classPart begin
                    MKAbsyn.PUBLIC(
                        contents=cElementContents
                    ) => cElementContents
                    MKAbsyn.PROTECTED(
                        contents=cElementContents
                    ) => cElementContents
                    _ => return list()
                end
                # search for classes
                for elementItem in elementItems
                    curClass = findClassInElementItem(elementItem)
                    if !isnothing(curClass)
                        # classes = ListUtil.append_reverse(classes, ListUtil.create(curClass))
                        classes = ListUtil.append_reverse(classes, Cons{Class}(curClass, list()))
                    end
                end


            end
            out = generateCodeFromClassList(classes)
        end
        _ => begin

            out = ""
        end

    end
    if out == ""
        return out
    end
    # get all classes in PARTS -> ClassPart --> ElementItem -> ELEMENT -> CLASSDEF
    # call createModelVariations with each class
    # return joined string
    return "package " + class.name + "\n" + out + "\nend " + class.name + ";\n"
end

function findClassInElementItem(elementItem::ElementItem)::Union{Class,Nothing}
    return @match elementItem begin
        MKAbsyn.ELEMENTITEM(
            element=eElement
        ) => findClassInElement(eElement)
        _ => nothing

    end
end

function findClassInElement(element::Element)::Union{Class,Nothing}
    return @match element begin
        MKAbsyn.ELEMENT(
            specification=eSpec
        ) => findClassInElementSpec(eSpec)
        _ => nothing

    end
end


function findClassInElementSpec(element::ElementSpec)::Union{Class,Nothing}
    # TODO: convert CLASS to class 
    return @match element begin
        MKAbsyn.CLASSDEF(
            class_=eClass
        ) => eClass
        _ => nothing

    end
end

function createInstanceName(contextName::String)::String
    lowercasefirst(contextName) + "_instance"
end


function getEquationItemsFromClass(inClass::MKAbsyn.Class)::List{EquationItem}

    cClassParts::List{ClassPart} = @match inClass.body begin
        MKAbsyn.PARTS(classParts=cClassParts) => cClassParts
        MKAbsyn.CLASS_EXTENDS(parts=cClassParts) => cClassParts
        _ => []
    end
    out::List{EquationItem} = list()
    for part in cClassParts
        equationItems = getEquationItemsFromClassPart(part)
        out = ListUtil.append_reverse(equationItems, out)
        # for equationItem in getEquationItemsFromClassPart(part)
        #     if equationItem !== nil
        #         ListUtil.append_reverse(out, ListUtil.create(equationItem))
        #     end
        # end
    end
    # println("cClassParts")
    # println(cClassParts)
    # println("=====out=====")
    # println(out)

    # # out = ListUtil.map()
    # println("classPartLists")
    # println(classPartLists)
    # println("classPartLists end")

    # return collect(Iterators.flatten(classPartLists))
    return out
end

function getEquationItemsFromClassPart(classPart::ClassPart)::List{EquationItem}
    return @match classPart begin
        MKAbsyn.EQUATIONS(contents=cContents) => cContents
        _ => nil
    end
end

function createModelVariation(inClass::MKAbsyn.Class, context_equation_sections::List{ContextEquationSystem}, contexts::List{ContextDeclaration})::String
    println("creating model variation for " + inClass.name)




    # check if ClassDef is of type PARTS or CLASS_EXTENDS (otherwise no variation possible)
    canBeVaried = false
    @match inClass.body begin
        MKAbsyn.PARTS(__) => begin
            canBeVaried = true
        end
        MKAbsyn.CLASS_EXTENDS(__) => begin
            canBeVaried = true
        end
    end

    if !canBeVaried
        println("class " + inClass.name + " cannot be varied (no PARTS or CLASS_EXTENDS) -> skipping variationing")
        return translateClass(MKAbsynProgramTraverser(), inClass)
    end


    # prepare outer model
    out = "model " + inClass.name + "\n"


    # create model list for each context
    

    # initial  model can be translated without any changes
    initialClassName = inClass.name + "__" + INITIAL_SUB_CLASS_NAME
    #initialSubClass = instantiateClassVariant(inClass, initialClassName, list())
    #push!(models, translateClass(MKAbsynProgramTraverser(), initialSubClass))

    # creating the dict mapping contexts to their condition and the list of models
    contextDict = Dict()
    models = []
    initialContextIncludesCondition = false
    for context in contexts
        equationItems::List{EquationItem} = getEquationItemsForContext(context_equation_sections, context.label)
        if equationItems == nil
            #TODO: handle case
            #continue
        end
        println("creating context class for " + context.label)
        isDefaultContext = cmp(context.label, "initial") == 0

        if isDefaultContext
            println("found initial context")
            model_id = initialClassName
            initialContextIncludesCondition = true
        else
            model_id = inClass.name + "_" + uppercasefirst(context.label)

        end
        subClass = instantiateClassVariant(inClass, model_id, equationItems)
        contextDict[model_id] = context.condition

        push!(models, translateClass(MKAbsynProgramTraverser(), subClass))
    end

    if (!initialContextIncludesCondition)
        println("no initial context found -> creating default context")
        # create default context
        initial_equations = getEquationItemsForContext(context_equation_sections, "initial")
        if context_equation_sections==nil
            throw(ArgumentError("No initial context found"))
        end
        subClass = instantiateClassVariant(inClass, initialClassName,initial_equations )
        push!(models, translateClass(MKAbsynProgramTraverser(), subClass))
        out += "structuralmode " + initialClassName + " " + createInstanceName(initialClassName) + ";\n"
    end

    # check if initial context exists



    # adding structuralmode header
    out += join(map(x -> "structuralmode " + x + " " + createInstanceName(x) + ";\n", collect(keys(contextDict))), "")





    # add everything except equations
    out += bundleElementsFromIterator((translateClassPart(MKAbsynProgramTraverser(), c, inClass) for c in getAllClassPartsExceptEquationsFromClassDef(inClass.body)))



    # add sub models
    out += join(models, "\n") + "\n"

    # add new equation section
    out += "equation\n"
    out += bundleElementsFromIterator((translateEquationItem(MKAbsynProgramTraverser(), c) for c in getEquationItemsFromClass(inClass)), true) + "\n"


    # add initialStructureMode
    out += "initialStructuralState(" + createInstanceName(initialClassName) + ");\n"

    # add transitions
    for (to_context, condition) in pairs(contextDict)


        # if to_context is default: we don't need to go from initial to initial
        # if cmp(to_context, initialClassName) !== 0
        #     out += "structuralTransition(" + createInstanceName(initialClassName) + ", " + createInstanceName(to_context) + ", " + translateExpression(MKAbsynProgramTraverser(), condition) + ");\n"
        # end

        for (from_context, _) in pairs(contextDict)
            println("==== NEW TO CONTEXT PAIR ====")
            println("checking " + from_context + " -> " + to_context)
            if (!initialContextIncludesCondition)
                out += "structuralTransition(" + createInstanceName(initialClassName) + ", " + createInstanceName(to_context) + ", " + translateExpression(MKAbsynProgramTraverser(), condition) + ");\n"
            end

            if cmp(from_context, to_context) == 0
                println("skipping because equal contexts")
                continue
            end

            # if cmp(from_context, initialClassName) == 0
            #     # no need for another transition from inital to current to_context
            #     println("skipping because initial context already there")
            #     continue
            # end

            println("adding transition")
            out += "structuralTransition(" + createInstanceName(from_context) + ", " + createInstanceName(to_context) + ", " + translateExpression(MKAbsynProgramTraverser(), condition) + ");\n"
        end
    end

    # finish super model
    out += "end " + inClass.name + ";\n"
    out
end

function getEquationItemsForContext(context_equation_sections::List{ContextEquationSystem}, context_label::String)::List{EquationItem}
    for context_equation_section in context_equation_sections
        if context_equation_section.contextLabel == context_label
            return context_equation_section.equations
        end
    end
    nil
end




function instantiateClassVariant(originalClass::Class, name::String, equationItems::List{EquationItem}=nothing)::Class

    newClassDef = originalClass.body
    if !isnothing(equationItems) && !isempty(equationItems)
        # replace all EQUATIONS from curClass.body
        newClassDef = replaceAndIsolateEquationsInClassDef(originalClass.body, equationItems)
    end


    newClass = CLASS(name, originalClass.partialPrefix, originalClass.finalPrefix, originalClass.encapsulatedPrefix, originalClass.restriction, newClassDef, originalClass.info)
    # todo: check every classPart if it is EQUATIONS


    newClass

end

function replaceAndIsolateEquationsInClassDef(classDef::ClassDef, equationItems::List{EquationItem})::ClassDef
    @match classDef begin
        MKAbsyn.PARTS(
            typeVars=cTypeVars,
            classAttrs=cAttrs,
            classParts=cParts,
            ann=cAnn,
            comment=cComment
        ) => begin
            MKAbsyn.PARTS(cTypeVars, cAttrs, generateClassPartListFromEquationItems(equationItems), cAnn, cComment)
        end
        MKAbsyn.CLASS_EXTENDS(
            baseClassName=cBaseClassName,
            modifications=cModifications,
            comment=cComment,
            parts=cParts,
            ann=cAnn
        ) => begin
            MKAbsyn.CLASS_EXTENDS(cBaseClassName, cModifications, cComment, generateClassPartListFromEquationItems(equationItems), cAnn)
        end
    end
end


function generateClassPartListFromEquationItems(equationItems::List{EquationItem})::List{ClassPart}
    ListUtil.create(convert(ClassPart, MKAbsyn.EQUATIONS(equationItems)))
end



function getAllClassPartsExceptEquationsFromClassDef(classDef::ClassDef)::List{ClassPart}
    @match classDef begin
        MKAbsyn.PARTS(
            typeVars=cTypeVars,
            classAttrs=cAttrs,
            classParts=cParts,
            ann=cAnn,
            comment=cComment
        ) => begin
            getAllClassPartsExceptEquationsFromClassParts(cParts)
        end
        MKAbsyn.CLASS_EXTENDS(
            baseClassName=cBaseClassName,
            modifications=cModifications,
            comment=cComment,
            parts=cParts,
            ann=cAnn
        ) => begin
            getAllClassPartsExceptEquationsFromClassParts(cParts)
        end
    end
end

function getAllClassPartsExceptEquationsFromClassParts(classParts::List{ClassPart})::List{ClassPart}
    out::List{ClassPart} = list()
    for classPart in classParts
        @match classPart begin
            MKAbsyn.EQUATIONS(__) => begin end
            MKAbsyn.CONTEXTDEFINITIONSECTION(__) => begin end
            MKAbsyn.CONTEXTEQUATIONS(__) => begin end
            _ => begin
                # keep all other classParts
                out = ListUtil.append_reverse(out, ListUtil.create(convert(ClassPart, classPart)))
            end
        end
    end
    out

end


end