module MKAbsynCodeGen

include("main.jl")

using ..MKAbsynProgramTraversal


using MKAbsyn

using ImmutableList
using MetaModelica
using ExportAll

import ListUtil

const INITIAL_SUB_CLASS_NAME = "Context_Initial"


function mockContextDeclarations(traverser::MKAbsynProgramTraverser, inClass::Class)
    translateContextDefinition(traverser, CONTEXTDEFINITION("sunny", RELATION(
            CREF(CREF_IDENT("x", Nil{Any}())),
            GREATER(),
            INTEGER(2),
        )), inClass)
    translateContextDefinition(traverser, CONTEXTDEFINITION("windy", RELATION(
            CREF(CREF_IDENT("x", Nil{Any}())),
            LESS(),
            INTEGER(0),
        )), inClass)
end

function generateCodeFromMKAbsyn(inProgram::MKAbsyn.Program)::String
    helper = MKAbsynProgramTraverser()
    resetState(helper)
    a = translateProgram(helper, inProgram)

    #TODO: remove this
    # mockContextDeclarations(helper, ListUtil.first(inProgram.classes))


    println("=======TRANSLATED PROGRAM==========")
    println(a)
    println("===================================")
    context_dict::Dict{Class,List{ContextDeclaration}} = helper.context_dict
    context_equation_dict::Dict{Class,List{ContextEquationSystem}} = helper.context_equation_dict

    # now context and context equation mappings are full
    # validate contexts and context equation mappings
    if !validate(context_equation_dict, context_dict)
        @error "Contexts not valid"
    end

    createModelVariations(inProgram, context_equation_dict, context_dict)
end

function validate(context_equation_dict::Dict{Class,List{ContextEquationSystem}}, context_dict::Dict{Class,List{ContextDeclaration}})::Bool
    #todo: check for duplicate labels in context definition
    #todo: check for duplicate labels in equation sections
    #todo: check for existence of all mentioned contexts in equation sections
    true
end


function createModelVariations(inProgram::MKAbsyn.Program, context_equation_dict::Dict{Class,List{ContextEquationSystem}}, context_dict::Dict{Class,List{ContextDeclaration}})::String
    out = []
    for class in inProgram.classes
        @match class.restriction begin
            MKAbsyn.R_CLASS() => begin
                push!(out, translateClass(MKAbsynProgramTraverser(), class))
            end
            MKAbsyn.R_MODEL() => begin
                push!(out, createModelVariation(class, context_equation_dict[class], context_dict[class]))
            end
        end
    end
    join(out, "\n")

end

function createInstanceName(contextName::String)::String
    lowercasefirst(contextName) + "_instance"
end

function createModelVariation(inClass::MKAbsyn.Class, context_equation_sections::List{ContextEquationSystem}, contexts::List{ContextDeclaration})::String
    println("creating model variation for " + inClass.name)

    # check if contexts exist
    if isempty(contexts) || isempty(context_equation_sections)
        println("no context or context equation sections for " + inClass.name + " -> skipping variationing")
        return translateClass(MKAbsynProgramTraverser(), class)
    end

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
    models = []

    # initial  model can be translated without any changes
    initialClassName = inClass.name + "__" + INITIAL_SUB_CLASS_NAME
    initialSubClass = instantiateClassVariant(inClass, initialClassName, list())
    push!(models, translateClass(MKAbsynProgramTraverser(), initialSubClass))

    # variations
    contextDict = Dict()
    for context in contexts
        equationItems::List{EquationItem} = getEquationItemsForContext(context_equation_sections, context)
        if equationItems == nil
            #TODO: handle case
            continue
        end
        model_id = inClass.name + "_" + uppercasefirst(context.label)
        contextDict[model_id] = context.condition

        subClass = instantiateClassVariant(inClass, model_id, equationItems)
        push!(models, translateClass(MKAbsynProgramTraverser(), subClass))
    end

    # add structural mode declarations for initial and contexts
    out += "structuralMode " + initialClassName + " " + createInstanceName(initialClassName) + ";\n"
    out += join(map(x -> "structuralMode " + x + " " + createInstanceName(x) + ";\n", collect(keys(contextDict))), "")

    # add sub models
    out += join(models, "\n") + "\n"

    # add new equation section
    out += "equation\n"

    # add initialStructureMode
    out += "initialStructuralState(" + createInstanceName(initialClassName) + ");\n"

    # add transitions
    for (from_context, _) in pairs(contextDict)
        for (to_context, condition) in pairs(contextDict)
            if from_context == to_context
                continue
            end
            out += "structuralTransition(" + createInstanceName(from_context) + ", " + createInstanceName(to_context) + ", " + translateExpression(MKAbsynProgramTraverser(), condition) + ");\n"
        end
    end

    # finish super model
    out += "end " + inClass.name + ";\n"
    out
end

function getEquationItemsForContext(context_equation_sections::List{ContextEquationSystem}, context::ContextDeclaration)::List{EquationItem}
    for context_equation_section in context_equation_sections
        if context_equation_section.contextLabel == context.label
            return context_equation_section.equations
        end
    end
    nil
end




function instantiateClassVariant(originalClass::Class, name::String, equationItems::List{EquationItem}=nothing)::Class

    newClassDef = originalClass.body
    if !isnothing(equationItems) && !isempty(equationItems)
        # remove all EQUATIONS from curClass.body
        newClassDef = replaceEquationsInClassDef(originalClass.body, equationItems)
    end


    newClass = CLASS(name, originalClass.partialPrefix, originalClass.finalPrefix, originalClass.encapsulatedPrefix, originalClass.restriction, newClassDef, originalClass.info)
    # todo: check every classPart if it is EQUATIONS


    newClass

end

function replaceEquationsInClassDef(classDef::ClassDef, equationItems::List{EquationItem})::ClassDef
    @match classDef begin
        MKAbsyn.PARTS(
            typeVars=cTypeVars,
            classAttrs=cAttrs,
            classParts=cParts,
            ann=cAnn,
            comment=cComment
        ) => begin
            MKAbsyn.PARTS(cTypeVars, cAttrs, replaceEquationsInClassParts(cParts, equationItems), cAnn, cComment)
        end
        MKAbsyn.CLASS_EXTENDS(
            baseClassName=cBaseClassName,
            modifications=cModifications,
            comment=cComment,
            parts=cParts,
            ann=cAnn
        ) => begin
            MKAbsyn.CLASS_EXTENDS(cBaseClassName, cModifications, cComment, replaceEquationsInClassParts(cParts, equationItems), cAnn)
        end
    end
end


function replaceEquationsInClassParts(classParts::List{ClassPart}, equationItems::List{EquationItem})::List{ClassPart}
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
    a::ClassPart = convert(ClassPart, MKAbsyn.EQUATIONS(equationItems))
    l::List{ClassPart} = ListUtil.create(a)
    out = ListUtil.append_reverse(out, l)
    out

end

end