module MKAbsynProgramTraversal



#= /*
* This file is part of OpenModelica.
*
* Copyright (c) 1998-2014, Open Source Modelica Consortium (OSMC),
* c/o Linköpings universitet, Department of Computer and Information Science,
* SE-58183 Linköping, Sweden.
*
* All rights reserved.
*
* THIS PROGRAM IS PROVIDED UNDER THE TERMS OF GPL VERSION 3 LICENSE OR
* THIS OSMC PUBLIC LICENSE (OSMC-PL) VERSION 1.2.
* ANY USE, REPRODUCTION OR DISTRIBUTION OF THIS PROGRAM CONSTITUTES
* RECIPIENT'S ACCEPTANCE OF THE OSMC PUBLIC LICENSE OR THE GPL VERSION 3,
* ACCORDING TO RECIPIENTS CHOICE.
*
* The OpenModelica software and the Open Source Modelica
* Consortium (OSMC) Public License (OSMC-PL) are obtained
* from OSMC, either from the above address,
* from the URLs: http:www.ida.liu.se/projects/OpenModelica or
* http:www.openmodelica.org, and in the OpenModelica distribution.
* GNU version 3 is obtained from: http:www.gnu.org/copyleft/gpl.html.
*
* This program is distributed WITHOUT ANY WARRANTY; without
* even the implied warranty of  MERCHANTABILITY or FITNESS
* FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH
* IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS OF OSMC-PL.
*
* See the full OSMC Public License conditions for more details.
*
*/ =#


using MetaModelica
using ExportAll

using MKAbsyn
import ListUtil

const ASSERTION_LEVEL_ERROR =
    MKAbsyn.CREF(MKAbsyn.CREF_FULLYQUALIFIED(MKAbsyn.CREF_QUAL(
        "AssertionLevel",
        nil,
        MKAbsyn.CREF_IDENT("error", nil),
    )))::MKAbsyn.Exp



# can be used a la bundleElementsFromIterator(c for c in [1,2,3])
function bundleElementsFromIterator(iter::Base.Generator, useSemicolon::Bool=false)::String
    join(map(a -> a + (useSemicolon ? ";" : ""), list(iter)), "\n")
end

mutable struct ContextDeclaration
    label::String
    condition::Exp
end

mutable struct ContextEquationSystem
    contextLabel::String
    equations::List{EquationItem}
end



mutable struct MKAbsynProgramTraverser

    context_dict::Dict{Class,List{ContextDeclaration}}
    context_equation_dict::Dict{Class,List{ContextEquationSystem}}

end

function MKAbsynProgramTraverser()
    return MKAbsynProgramTraverser(Dict(), Dict())
end

function resetState(self::MKAbsynProgramTraverser)
    self.context_dict = Dict()
    self.context_equation_dict = Dict()
end


function translateProgram(self::MKAbsynProgramTraverser, inProgram::MKAbsyn.Program)::String
    @match inProgram begin
        _ => begin
            bundleElementsFromIterator(translateClass(self, c) for c in inProgram.classes)
        end
    end
end

function translateClass(self::MKAbsynProgramTraverser, inClass::MKAbsyn.Class)::String

    local cName::String
    local cPartialPrefix::Bool
    local cFinalPrefix::Bool
    local cEncapsulatedPrefix::Bool
    local cRestriction::MKAbsyn.Restriction
    local cBody::MKAbsyn.ClassDef
    local cInfo::MKAbsyn.Info


    @match inClass begin
        MKAbsyn.CLASS(
            name=cName,
            partialPrefix=cPartialPrefix,
            finalPrefix=cFinalPrefix,
            encapsulatedPrefix=cEncapsulatedPrefix,
            restriction=cRestriction,
            body=cBody,
            info=cInfo,
        ) => begin
            classDef = translateClassDef(self, cBody, inClass)
            restStr = begin
                @match cRestriction begin
                    MKAbsyn.R_MODEL() => "model"
                    MKAbsyn.R_CLASS() => "class"
                    MKAbsyn.R_PACKAGE() => "package"
                end
            end
            restStr + " " + cName + "\n" + classDef + "\n" + "end " + cName + ";"
        end
    end
end

function translateClassDef(
    self::MKAbsynProgramTraverser,
    inClassDef::MKAbsyn.ClassDef,
    inClass::MKAbsyn.Class
)::String
    local dClassParts::List{MKAbsyn.ClassPart}
    local dComment::Option{String}
    local typeVars::List{String}


    @match inClassDef begin
        # PARTS
        MKAbsyn.PARTS(
            typeVars=typeVars,
            classAttrs=classAttrs,
            classParts=dClassParts,
            ann=ann,
            comment=cmtString,
        ) => bundleElementsFromIterator(translateClassPart(self, c, inClass) for c in dClassParts)
        #TODO: OTHERS 
    end

end



function translateClassPart(self::MKAbsynProgramTraverser, inClassPart::MKAbsyn.ClassPart, inClass::MKAbsyn.Class)::String
    local cEquationContents::List{EquationItem}
    local cElementContents::List{ElementItem}

    @match inClassPart begin
        MKAbsyn.EQUATIONS(
            contents=cEquationContents
        ) => "equation\n" + bundleElementsFromIterator((translateEquationItem(self, c) for c in cEquationContents), true)
        MKAbsyn.PUBLIC(
            contents=cElementContents
        ) => bundleElementsFromIterator((translateElementItem(self, c) for c in cElementContents), true)
        MKAbsyn.CONTEXTEQUATIONS(
            label=cLabel,
            contents=cEquationContents
        ) => translateContextEquations(self, cLabel, cEquationContents, inClass)
        CONTEXTDEFINITIONSECTION(
            contents=cContextDefinitions,
        ) => begin
            foreach(c -> translateContextDefinition(self, c, inClass), cContextDefinitions)
            ""
        end

        #TODO: OTHERS 
    end
end

function translateContextDefinition(self::MKAbsynProgramTraverser, contextDefinition::ContextDefinition, inClass::MKAbsyn.Class)::String
    @match contextDefinition begin
        MKAbsyn.CONTEXTDEFINITION(
            label=cLabel,
            condition=cCondition
        ) => begin
            appendingList = ListUtil.create(ContextDeclaration(cLabel, cCondition))
            if haskey(self.context_dict, inClass)
                # append to list
                println("extend entry for class")
                self.context_dict[inClass] = ListUtil.append_reverse(self.context_dict[inClass], appendingList)
            else
                # create new list
                println("new entry for class")
                self.context_dict[inClass] = appendingList
            end
        end

        #TODO: OTHERS 
    end
    ""
end

function translateContextEquations(self::MKAbsynProgramTraverser, label::String, contents::List{EquationItem}, inClass::MKAbsyn.Class)::String
    # check if inClass already in dict
    appendingList = ListUtil.create(ContextEquationSystem(label, contents))
    if haskey(self.context_equation_dict, inClass)
        # append to list
        println("extend entry for class")
        self.context_equation_dict[inClass] = ListUtil.append_reverse(self.context_equation_dict[inClass], appendingList)
    else
        # create new list
        println("new entry for class")
        self.context_equation_dict[inClass] = appendingList
    end
    ""
end

function translateEquationItem(self::MKAbsynProgramTraverser, equation::MKAbsyn.EquationItem)::String
    local cEquation::Equation #= equation =#
    local cComment::Option{Comment} #= comment =#
    local cInfo::Info #= line number =#
    @match equation begin
        MKAbsyn.EQUATIONITEM(
            equation_=cEquation,
            comment=cComment,
            info=cInfo
        ) => begin
            #TODO: add comment?
            #TODO: add info
            translateEquation(self, cEquation)
        end
        EQUATIONITEMCOMMENT(
            comment=cComment,
        ) => begin
            strip(cComment)
        end
    end
end

function translateElementItem(self::MKAbsynProgramTraverser, element::MKAbsyn.ElementItem)::String
    @match element begin
        MKAbsyn.ELEMENTITEM(
            element=eElement
        ) => translateElement(self, eElement)
        MKAbsyn.LEXER_COMMENT(
            comment=eComment
        ) => strip(eComment)
    end
end

function translateElement(self::MKAbsynProgramTraverser, element::MKAbsyn.Element)::String
    @match element begin
        MKAbsyn.ELEMENT(
            specification=eSpecification
        ) => translateElementSpec(self, eSpecification)
    end
end

function translateElementSpec(self::MKAbsynProgramTraverser, elementSpec::MKAbsyn.ElementSpec)::String
    @match elementSpec begin
        MKAbsyn.COMPONENTS(
            attributes=eAttributes,
            typeSpec=eTypeSpec,
            components=eComponents
        ) => begin
            translateTypeSpec(self, eTypeSpec) + " " + join((translateComponentItem(self, c) for c in eComponents), ", ")
        end
        MKAbsyn.CLASSDEF(
            replaceable_=eReplaceable,
            class_=eClass,
        ) => begin
            translateClass(self, eClass)
        end
    end
end

function translateTypeSpec(self::MKAbsynProgramTraverser, typeSpec::MKAbsyn.TypeSpec)::String
    @match typeSpec begin
        MKAbsyn.TPATH(
            path=tPath,
            arrayDim=tArrayDim
        ) => begin
            translatePath(self, tPath)
        end
    end
end

function translatePath(self::MKAbsynProgramTraverser, path::MKAbsyn.Path)::String
    @match path begin
        MKAbsyn.IDENT(
            name=pName
        ) => begin
            String(pName)
        end
    end
end

function translateComponentItem(self::MKAbsynProgramTraverser, component::MKAbsyn.ComponentItem)::String
    @match component begin
        MKAbsyn.COMPONENTITEM(
            component=cComponent,
            condition=cCondition,
            comment=cComment
        ) => begin
            translateComponent(self, cComponent)
        end
    end
end

function translateComponent(self::MKAbsynProgramTraverser, component::MKAbsyn.Component)::String
    @match component begin
        MKAbsyn.COMPONENT(
            name=cName,
            arrayDim=cArrayDim,
            modification=cModification
        ) => begin
            cName
        end
    end
end

function translateEquation(self::MKAbsynProgramTraverser, equation::MKAbsyn.Equation)::String

    local eLeftSide::MKAbsyn.Exp
    local eRightSide::MKAbsyn.Exp

    @match equation begin
        MKAbsyn.EQ_EQUALS(
            leftSide=eLeftSide,
            rightSide=eRightSide
        ) => begin
            translateExpression(self, eLeftSide) + " = " + translateExpression(self, eRightSide)
        end
        MKAbsyn.EQ_WHEN_E(
            whenExp=eWhenExp,
            whenEquations=eWhenEquations,
            elseWhenEquations=eElseWhenEquations
        ) => begin
            "when " + translateExpression(self, eWhenExp) + " then\n" + bundleElementsFromIterator((translateEquationItem(self, c) for c in eWhenEquations), true) + "\nelse\n" + bundleElementsFromIterator((translateEquationItem(self, c) for c in eElseWhenEquations), true) + "\nend when"
        end

    end
end

function translateExpression(self::MKAbsynProgramTraverser, exp::Exp)
    @match exp begin
        MKAbsyn.INTEGER(
            value=eValue
        ) => begin
            String(eValue)
        end
        MKAbsyn.REAL(
            value=eValue
        ) => begin
            String(eValue)
        end
        MKAbsyn.BOOL(
            value=eValue
        ) => begin
            String(eValue)
        end
        MKAbsyn.STRING(
            value=eValue
        ) => begin
            eValue
        end
        MKAbsyn.CREF(
            componentRef=eComponentRef
        ) => begin
            translateComponentRef(self, eComponentRef)
        end
        MKAbsyn.CALL(
            function_=eFunction,
            functionArgs=eFunctionArgs,
            typeVars=eTypeVars
        ) => begin
            translateComponentRef(self, eFunction) + translateFunctionArgs(self, eFunctionArgs)
        end
        MKAbsyn.BINARY(
            exp1=eExp1,
            op=eOp,
            exp2=eExp2
        ) => begin
            "(" + translateExpression(self, eExp1) + " " + translateBinaryOperator(self, eOp) + " " + translateExpression(self, eExp2) + ")"
        end
        MKAbsyn.RELATION(
            exp1=eExp1,
            op=eOp,
            exp2=eExp2
        ) => begin
            translateExpression(self, eExp1) + " " + translateBinaryOperator(self, eOp) + " " + translateExpression(self, eExp2)
        end

        MKAbsyn.LBINARY(
            exp1=eExp1,
            op=eOp,
            exp2=eExp2
        ) => begin
            translateExpression(self, eExp1) + " " + translateBinaryOperator(self, eOp) + " " + translateExpression(self, eExp2)
        end
    end
end

function translateBinaryOperator(self::MKAbsynProgramTraverser, operator::MKAbsyn.Operator)::String
    @match operator begin
        MKAbsyn.ADD() => "+"
        MKAbsyn.SUB() => "-"
        MKAbsyn.MUL() => "*"
        MKAbsyn.DIV() => "/"
        MKAbsyn.LESS() => "<"
        MKAbsyn.LESSEQ() => "<="
        MKAbsyn.GREATER() => ">"
        MKAbsyn.GREATEREQ() => ">="
        MKAbsyn.AND() => "and"
        MKAbsyn.OR() => "or"
        MKAbsyn.EQUAL() => "=="

    end
end


function translateFunctionArgs(self::MKAbsynProgramTraverser, functionArgs::FunctionArgs)::String
    @match functionArgs begin
        MKAbsyn.FUNCTIONARGS(
            args=fArgs,
            argNames=fArgNames
        ) => begin
            "(" + join((translateExpression(self, a) for a in fArgs), ",") + ")"
        end
    end

end

function translateComponentRef(self::MKAbsynProgramTraverser, componentRef::MKAbsyn.ComponentRef)::String

    @match componentRef begin
        MKAbsyn.CREF_IDENT(
            name=cName,
            subscripts=cSubscripts
        ) => begin
            cName
        end
        MKAbsyn.CREF_QUAL(
            name=cName,
            subscripts=cSubscripts,
            componentRef=cComponentRef
        ) => begin
            cName + "." + translateComponentRef(self, cComponentRef)
        end

    end
end


@exportAll
end
