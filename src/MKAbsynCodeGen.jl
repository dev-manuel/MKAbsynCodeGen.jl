module MKAbsynCodeGen

include("main.jl")

using ..MKAbsynProgramTraversal


using MKAbsyn

using ImmutableList
using MetaModelica
using ExportAll

import ListUtil

const INITIAL_SUB_CLASS_NAME = "context_initial"

function generateCodeFromMKAbsyn(inProgram::MKAbsyn.Program)::Dict{Class,List{ContextEquationSystem}}

    println("a")
    helper = MKAbsynProgramTraverser()
    resetState(helper)
    a = translateProgram(helper, inProgram)
    println(a)
    context_dict::Dict{Class,List{ContextDeclaration}} = helper.context_dict
    context_equation_dict::Dict{Class,List{ContextEquationSystem}} = helper.context_equation_dict
    return context_equation_dict

    # now context and context equation mappings are full
    # validate contexts and context equation mappings
    if !validate()
        @error "Contexts not valid"
    end

    createModelVariations(inProgram, context_equation_dict, context_dict)
end

function validate()::Bool
    #todo: check for duplicate labels in context definition
    #todo: check for existence of all mentioned contexts in equation sections
    true
end


function createModelVariations(inProgram::MKAbsyn.Program, context_equation_dict::Dict{Class,List{ContextEquationSystem}}, context_dict::Dict{Class,List{ContextDeclaration}})::String
    out = list()
    for class in inProgram.classes
        if isa(class.restriction, MKAbsyn.Restriction.R_CLASS)
            push!(out, MKAbsynProgramTraverser().translateClass(class))
        else
            push!(out, createModelVariation(class, context_equation_dict[class], context_dict[class]))
        end
    end
    join(out, "\n")

end

function createModelVariation(inClass::MKAbsyn.Class, context_equation_sections::List{ContextEquationSystem}, contexts::List{ContextDeclaration})::String

    if isempty(contexts) || isempty(context_equation_sections)
        return MKAbsynProgramTraverser().translateClass(class)
    end

    out = "model " + inClass.name

    # create model for each context
    models = list()
    # initial  model
    initialSubClass = deepcopy(inClass)
    initialSubClass.name = INITIAL_SUB_CLASS_NAME
    push!(models, MKAbsynProgramTraverser().translateClass(initialSubClass))

    # variations
    contextDict = Dict()
    counter = 0
    for context in contexts
        equationItems::List{EquationItem} = getEquationItemsForContext(context_equation_sections, context)
        if equationItems == nil
            continue
        end
        model_id = "context_" + counter
        contextDict[context] = model_id
        counter += 1

        subClass = createSubClass(inClass, equationItems, model_id)
        push!(models, MKAbsynProgramTraverser().translateClass(subClass))
    end


    out += join(map(x -> "structuralMode " + x, collect(values(contextDict)), "\n")) + "\n"

    out += join(models, "\n")


    # add initialStructureMode
    out += "initialStructuralState(" + INITIAL_SUB_CLASS_NAME + ")\n"

    # add structuralTransition
    for (from_context, from_label) in pairs(contextDict)
        for (to_context, to_label) in pairs(contextDict)
            if from_label == to_label
                continue
            end
            out += "structuralTransition(" + from_label + "," + to_label + "," + MKAbsynProgramTraverser().translateExpression(to_context.condition) + ")\n"
        end
    end

    # finish super model
    out += "end " + inClass.name

end

function getEquationItemsForContext(context_equation_sections::List{ContextEquationSystem}, context::ContextDeclaration)::List{EquationItem}
    for context_equation_section in context_equation_sections
        if context_equation_section.label == context.label
            return context_equation_section.contents
        end
    end
    nil
end



function createSubClass(originalClass::Class, equationItems::List{EquationItem}, name::String)::Class
    # create copy of class
    curClass = deepcopy(originalClass)
    curClass.name = name
    #todo: check if body is PARTS
    # todo: check every classPart if it is EQUATIONS

    curClass.body.classParts


end

end