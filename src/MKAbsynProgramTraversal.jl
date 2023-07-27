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
function bundleElementsFromIterator(iter::Base.Generator)::String
    join(list(iter), "\n")
end

mutable struct ContextDeclaration
    label::String
    condition::List{Exp}
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
            classDef = translateClassDef(self, cBody, cInfo, cRestriction, inClass)
            restStr = begin
                @match cRestriction begin
                    MKAbsyn.R_MODEL() => "model"
                    MKAbsyn.R_CLASS() => "class"
                end
            end
            restStr + " " + cName + "\n" + classDef
        end
    end
end

function translateClassDef(
    self::MKAbsynProgramTraverser,
    inClassDef::MKAbsyn.ClassDef,
    info::MKAbsyn.SourceInfo,
    re::MKAbsyn.Restriction,
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
        ) => "equation\n" + bundleElementsFromIterator(translateEquationItem(self, c) for c in cEquationContents)
        MKAbsyn.PUBLIC(
            contents=cElementContents
        ) => bundleElementsFromIterator(translateElementItem(self, c) for c in cElementContents)
        MKAbsyn.PUBLIC(
            contents=cElementContents
        ) => bundleElementsFromIterator(translateElementItem(self, c) for c in cElementContents)
        MKAbsyn.CONTEXTEQUATIONS(
            label=cLabel,
            contents=cEquationContents
        ) => translateContextEquations(self, cLabel, cEquationContents, inClass)

        #TODO: OTHERS 
    end
end

function translateContextEquations(self::MKAbsynProgramTraverser, label::String, contents::List{EquationItem}, inClass::MKAbsyn.Class)::String
    # check if inClass already in dict
    println("got you")
    if haskey(self.context_equation_dict, inClass)
        # append to list
        self.context_equation_dict[inClass] = self.context_equation_dict[inClass] + ContextEquationSystem(label, contents)
    else
        # create new list
        self.context_equation_dict[inClass] = [ContextEquationSystem(label, contents)]
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
    end
end

function translateElementItem(self::MKAbsynProgramTraverser, element::MKAbsyn.ElementItem)::String
    @match element begin
        MKAbsyn.ELEMENTITEM(
            element=eElement
        ) => translateElement(self, eElement)
        MKAbsyn.LEXER_COMMENT(
            comment=eComment
        ) => "//" + eComment
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
    end
end

#     #= mahge: FIX HERE. Check for proper input and output
#     =#
#     #= declarations in operators according to the specifications.
#     =#

#     function translateOperatorDef(
#         inClassDef::MKAbsyn.ClassDef,
#         operatorName::MKAbsyn.Ident,
#         info::SourceInfo,
#     )::Tuple{SCode.ClassDef,SCode.Comment}
#         local cmt::SCode.Comment
#         local outOperDef::SCode.ClassDef

#         (outOperDef, cmt) = begin
#             local cmtString::Option{String}
#             local els::List{SCode.Element}
#             local anns::List{SCode.Annotation}
#             local parts::List{MKAbsyn.ClassPart}
#             local scodeCmt::Option{SCode.Comment}
#             local opName::SCode.Ident
#             local aann::List{MKAbsyn.Annotation}
#             local ann::Option{SCode.Annotation}
#             @match (inClassDef, operatorName, info) begin
#                 (MKAbsyn.PARTS(classParts=parts, ann=aann, comment=cmtString), _, _) => begin
#                     els = translateClassdefElements(parts)
#                     cmt = translateCommentList(aann, cmtString)
#                     (SCode.PARTS(els, nil, nil, nil, nil, nil, nil, NONE()), cmt)
#                 end

#                 _ => begin
#                     fail()
#                 end
#             end
#         end
#         return (outOperDef, cmt)
#     end

#     function getOperatorGivenName(inOperatorFunction::SCode.Element)::MKAbsyn.Path
#         local outName::MKAbsyn.Path

#         outName = begin
#             local name::SCode.Ident
#             @match inOperatorFunction begin
#                 SCode.CLASS(name, _, _, _, SCode.R_FUNCTION(SCode.FR_OPERATOR_FUNCTION(__)), _, _, _) => begin
#                     MKAbsyn.IDENT(name)
#                 end
#             end
#         end
#         return outName
#     end

#     function getOperatorQualName(
#         inOperatorFunction::SCode.Element,
#         operName::SCode.Ident,
#     )::SCode.Path
#         local outName::SCode.Path

#         outName = begin
#             local name::SCode.Ident
#             local opname::SCode.Ident
#             @match (inOperatorFunction, operName) begin
#                 (SCode.CLASS(name, _, _, _, SCode.R_FUNCTION(_), _, _, _), opname) => begin
#                     MKAbsynUtil.joinPaths(MKAbsyn.IDENT(opname), MKAbsyn.IDENT(name))
#                 end
#             end
#         end
#         return outName
#     end

#     function getListofQualOperatorFuncsfromOperator(inOperator::SCode.Element)::List{SCode.Path}
#         local outNames::List{SCode.Path}

#         outNames = begin
#             local els::List{SCode.Element}
#             local opername::SCode.Ident
#             local names::List{SCode.Path}
#             #= If operator get the list of functions in it.
#             =#
#             @match inOperator begin
#                 SCode.CLASS(
#                     opername,
#                     _,
#                     _,
#                     _,
#                     SCode.R_OPERATOR(__),
#                     SCode.PARTS(elementLst=els),
#                     _,
#                     _,
#                 ) => begin
#                     names = ListUtil.map1(els, getOperatorQualName, opername)
#                     names
#                 end

#                 SCode.CLASS(
#                     opername,
#                     _,
#                     _,
#                     _,
#                     SCode.R_FUNCTION(SCode.FR_OPERATOR_FUNCTION(__)),
#                     _,
#                     _,
#                     _,
#                 ) => begin
#                     names = list(MKAbsyn.IDENT(opername))
#                     names
#                 end
#             end
#         end
#         #= If operator function return its name
#         =#
#         return outNames
#     end

#     function translatePurity(inPurity::MKAbsyn.FunctionPurity)::Bool
#         local outPurity::Bool

#         outPurity = begin
#             @match inPurity begin
#                 MKAbsyn.IMPURE(__) => begin
#                     true
#                 end

#                 _ => begin
#                     false
#                 end
#             end
#         end
#         return outPurity
#     end

#     #=  Changed to public! krsta
#     =#

#     """ #= Convert a class restriction. =#"""
#     function translateRestriction(
#         inClass::MKAbsyn.Class,
#         inRestriction::MKAbsyn.Restriction,
#     )::SCode.Restriction
#         local outRestriction::SCode.Restriction

#         outRestriction = begin
#             local d::MKAbsyn.Class
#             local name::MKAbsyn.Path
#             local index::Integer
#             local singleton::Bool
#             local isImpure::Bool
#             local moved::Bool
#             local purity::MKAbsyn.FunctionPurity
#             local typeVars::List{String}
#             #=  ?? Only normal functions can have 'external'
#             =#
#             @match (inClass, inRestriction) begin
#                 (d, MKAbsyn.R_FUNCTION(MKAbsyn.FR_NORMAL_FUNCTION(purity))) => begin
#                     isImpure = translatePurity(purity)
#                     if containsExternalFuncDecl(d)
#                         SCode.R_FUNCTION(SCode.FR_EXTERNAL_FUNCTION(isImpure))
#                     else
#                         SCode.R_FUNCTION(SCode.FR_NORMAL_FUNCTION(isImpure))
#                     end
#                 end

#                 (_, MKAbsyn.R_FUNCTION(MKAbsyn.FR_OPERATOR_FUNCTION(__))) => begin
#                     SCode.R_FUNCTION(SCode.FR_OPERATOR_FUNCTION())
#                 end

#                 (_, MKAbsyn.R_FUNCTION(MKAbsyn.FR_PARALLEL_FUNCTION(__))) => begin
#                     SCode.R_FUNCTION(SCode.FR_PARALLEL_FUNCTION())
#                 end

#                 (_, MKAbsyn.R_FUNCTION(MKAbsyn.FR_KERNEL_FUNCTION(__))) => begin
#                     SCode.R_FUNCTION(SCode.FR_KERNEL_FUNCTION())
#                 end

#                 (_, MKAbsyn.R_CLASS(__)) => begin
#                     SCode.R_CLASS()
#                 end

#                 (_, MKAbsyn.R_OPTIMIZATION(__)) => begin
#                     SCode.R_OPTIMIZATION()
#                 end

#                 (_, MKAbsyn.R_MODEL(__)) => begin
#                     SCode.R_MODEL()
#                 end

#                 (_, MKAbsyn.R_RECORD(__)) => begin
#                     SCode.R_RECORD(false)
#                 end

#                 (_, MKAbsyn.R_OPERATOR_RECORD(__)) => begin
#                     SCode.R_RECORD(true)
#                 end

#                 (_, MKAbsyn.R_BLOCK(__)) => begin
#                     SCode.R_BLOCK()
#                 end

#                 (_, MKAbsyn.R_CONNECTOR(__)) => begin
#                     SCode.R_CONNECTOR(false)
#                 end

#                 (_, MKAbsyn.R_EXP_CONNECTOR(__)) => begin
#                     System.setHasExpandableConnectors(true)
#                     SCode.R_CONNECTOR(true)
#                 end

#                 (_, MKAbsyn.R_OPERATOR(__)) => begin
#                     SCode.R_OPERATOR()
#                 end

#                 (_, MKAbsyn.R_TYPE(__)) => begin
#                     SCode.R_TYPE()
#                 end

#                 (_, MKAbsyn.R_PACKAGE(__)) => begin
#                     SCode.R_PACKAGE()
#                 end

#                 (_, MKAbsyn.R_ENUMERATION(__)) => begin
#                     SCode.R_ENUMERATION()
#                 end

#                 (_, MKAbsyn.R_PREDEFINED_INTEGER(__)) => begin
#                     SCode.R_PREDEFINED_INTEGER()
#                 end

#                 (_, MKAbsyn.R_PREDEFINED_REAL(__)) => begin
#                     SCode.R_PREDEFINED_REAL()
#                 end

#                 (_, MKAbsyn.R_PREDEFINED_STRING(__)) => begin
#                     SCode.R_PREDEFINED_STRING()
#                 end

#                 (_, MKAbsyn.R_PREDEFINED_BOOLEAN(__)) => begin
#                     SCode.R_PREDEFINED_BOOLEAN()
#                 end

#                 (_, MKAbsyn.R_PREDEFINED_CLOCK(__)) => begin
#                     SCode.R_PREDEFINED_CLOCK()
#                 end

#                 (_, MKAbsyn.R_PREDEFINED_ENUMERATION(__)) => begin
#                     SCode.R_PREDEFINED_ENUMERATION()
#                 end

#                 (_, MKAbsyn.R_METARECORD(name, index, singleton, moved, typeVars)) => begin
#                     SCode.R_METARECORD(name, index, singleton, moved, typeVars)
#                 end

#                 (MKAbsyn.CLASS(body=MKAbsyn.PARTS(typeVars=typeVars)), MKAbsyn.R_UNIONTYPE(__)) =>
#                     begin
#                         SCode.R_UNIONTYPE(typeVars)
#                     end

#                 (_, MKAbsyn.R_UNIONTYPE(__)) => begin
#                     SCode.R_UNIONTYPE(nil)
#                 end
#             end
#         end
#         #=  BTH
#         =#
#         #= MetaModelica extension, added by x07simbj
#         =#
#         #= /*MetaModelica extension added by x07simbj */ =#
#         #= /*MetaModelica extension added by x07simbj */ =#
#         return outRestriction
#     end

#     """ #= Returns true if the MKAbsyn.Class contains an external function declaration. =#"""
#     function containsExternalFuncDecl(inClass::MKAbsyn.Class)::Bool
#         local outBoolean::Bool

#         outBoolean = begin
#             local res::Bool
#             local b::Bool
#             local c::Bool
#             local d::Bool
#             local a::String
#             local e::MKAbsyn.Restriction
#             local rest::List{MKAbsyn.ClassPart}
#             local cmt::Option{String}
#             local file_info::SourceInfo
#             local ann::List{MKAbsyn.Annotation}
#             @match inClass begin
#                 MKAbsyn.CLASS(body=MKAbsyn.PARTS(classParts=MKAbsyn.EXTERNAL(__) <| _)) => begin
#                     true
#                 end

#                 MKAbsyn.CLASS(
#                     name=a,
#                     partialPrefix=b,
#                     finalPrefix=c,
#                     encapsulatedPrefix=d,
#                     restriction=e,
#                     body=MKAbsyn.PARTS(classParts=_ <| rest, comment=cmt, ann=ann),
#                     info=file_info,
#                 ) => begin
#                     containsExternalFuncDecl(MKAbsyn.CLASS(
#                         a,
#                         b,
#                         c,
#                         d,
#                         e,
#                         MKAbsyn.PARTS(nil, nil, rest, ann, cmt),
#                         file_info,
#                     ))
#                 end

#                 MKAbsyn.CLASS(body=MKAbsyn.CLASS_EXTENDS(parts=MKAbsyn.EXTERNAL(__) <| _)) => begin
#                     true
#                 end

#                 MKAbsyn.CLASS(
#                     name=a,
#                     partialPrefix=b,
#                     finalPrefix=c,
#                     encapsulatedPrefix=d,
#                     restriction=e,
#                     body=MKAbsyn.CLASS_EXTENDS(parts=_ <| rest, comment=cmt, ann=ann),
#                     info=file_info,
#                 ) => begin
#                     containsExternalFuncDecl(MKAbsyn.CLASS(
#                         a,
#                         b,
#                         c,
#                         d,
#                         e,
#                         MKAbsyn.PARTS(nil, nil, rest, ann, cmt),
#                         file_info,
#                     ))
#                 end

#                 _ => begin
#                     false
#                 end
#             end
#         end
#         #= /* adrpo: handling also the case model extends X external ... end X; */ =#
#         #= /* adrpo: handling also the case model extends X external ... end X; */ =#
#         return outBoolean
#     end

#     """ @author: adrpo
#         translates from MKAbsyn.ElementAttributes to SCode.Attributes
#     """
#     function translateAttributes(
#         inEA::MKAbsyn.ElementAttributes,
#         extraArrayDim::List{<:MKAbsyn.Subscript},
#     )::SCode.Attributes
#         local outA::SCode.Attributes
#         outA = begin
#             local f::Bool
#             local s::Bool
#             local v::MKAbsyn.Variability
#             local p::MKAbsyn.Parallelism
#             local adim::MKAbsyn.ArrayDim
#             local extraADim::MKAbsyn.ArrayDim
#             local dir::MKAbsyn.Direction
#             local fi::MKAbsyn.IsField
#             local ct::SCode.ConnectorType
#             local sp::SCode.Parallelism
#             local sv::SCode.Variability
#             @match (inEA, extraArrayDim) begin
#                 (MKAbsyn.ATTR(f, s, p, v, dir, fi, adim, mo), extraADim) => begin
#                     ct = translateConnectorType(f, s)
#                     sv = translateVariability(v)
#                     sp = translateParallelism(p)
#                     adim = listAppend(extraADim, adim)
#                     @debug "Value of mode in translate Attributes" mo
#                     SCode.ATTR(adim, ct, sp, sv, dir, fi, mo)
#                 end
#             end
#         end
#         return outA
#     end

#     function translateConnectorType(inFlow::Bool, inStream::Bool)::SCode.ConnectorType
#         local outType::SCode.ConnectorType

#         outType = begin
#             @match (inFlow, inStream) begin
#                 (false, false) => begin
#                     SCode.POTENTIAL()
#                 end

#                 (true, false) => begin
#                     SCode.FLOW()
#                 end

#                 (false, true) => begin
#                     SCode.STREAM()
#                 end

#                 (true, true) => begin
#                     #=  Both flow and stream is not allowed by the grammar, so this shouldn't be  possible. =#
#                     fail()
#                 end
#             end
#         end
#         return outType
#     end



#     """ #= first class annotation instead, since it is very common that an element
#     annotation is used for this purpose.
#     For instance, instead of external \\\"C\\\" annotation(Library=\\\"foo.lib\\\";
#     it says external \\\"C\\\" ; annotation(Library=\\\"foo.lib\\\"; =#"""
#     function translateAlternativeExternalAnnotation(
#         decl::Option{<:SCode.ExternalDecl},
#         comment::SCode.Comment,
#     )::Option{SCode.ExternalDecl}
#         local outDecl::Option{SCode.ExternalDecl}

#         outDecl = begin
#             local name::Option{SCode.Ident}
#             local l::Option{String}
#             local out::Option{MKAbsyn.ComponentRef}
#             local a::List{MKAbsyn.Exp}
#             local ann1::Option{SCode.Annotation}
#             local ann2::Option{SCode.Annotation}
#             local ann::Option{SCode.Annotation}
#             #=  none
#             =#
#             @match (decl, comment) begin
#                 (NONE(), _) => begin
#                     NONE()
#                 end

#                 (
#                     SOME(SCode.EXTERNALDECL(name, l, out, a, ann1)),
#                     SCode.COMMENT(annotation_=ann2),
#                 ) => begin
#                     ann = SCodeUtil.mergeSCodeOptAnn(ann1, ann2)
#                     SOME(SCode.EXTERNALDECL(name, l, out, a, ann))
#                 end
#             end
#         end
#         #=  Else, merge
#         =#
#         return outDecl
#     end

#     function mergeSCodeAnnotationsFromParts(
#         part::MKAbsyn.ClassPart,
#         inMod::Option{<:SCode.Annotation},
#     )::Option{SCode.Annotation}
#         local outMod::Option{SCode.Annotation}

#         outMod = begin
#             local aann::MKAbsyn.Annotation
#             local ann::Option{SCode.Annotation}
#             local rest::List{MKAbsyn.ElementItem}
#             @match (part, inMod) begin
#                 (MKAbsyn.EXTERNAL(_, SOME(aann)), _) => begin
#                     ann = translateAnnotation(aann)
#                     ann = SCodeUtil.mergeSCodeOptAnn(ann, inMod)
#                     ann
#                 end

#                 (MKAbsyn.PUBLIC(_ <| rest), _) => begin
#                     mergeSCodeAnnotationsFromParts(MKAbsyn.PUBLIC(rest), inMod)
#                 end

#                 (MKAbsyn.PROTECTED(_ <| rest), _) => begin
#                     mergeSCodeAnnotationsFromParts(MKAbsyn.PROTECTED(rest), inMod)
#                 end

#                 _ => begin
#                     inMod
#                 end
#             end
#         end
#         return outMod
#     end

#     """ #= Convert an EnumLiteral list to an Ident list.
#     Comments are lost. =#"""
#     function translateEnumlist(
#         inMKAbsynEnumLiteralLst::List{<:MKAbsyn.EnumLiteral},
#     )::List{SCode.Enum}
#         local outEnumLst::List{SCode.Enum}

#         outEnumLst = begin
#             local res::List{SCode.Enum}
#             local id::String
#             local cmtOpt::Option{MKAbsyn.Comment}
#             local cmt::SCode.Comment
#             local rest::List{MKAbsyn.EnumLiteral}
#             @match inMKAbsynEnumLiteralLst begin
#                 nil() => begin
#                     nil
#                 end

#                 MKAbsyn.ENUMLITERAL(id, cmtOpt) <| rest => begin
#                     cmt = translateComment(cmtOpt)
#                     res = translateEnumlist(rest)
#                     _cons(SCode.ENUM(id, cmt), res)
#                 end
#             end
#         end
#         return outEnumLst
#     end

#     """ #= Convert an MKAbsyn.ClassPart list to an Element list. =#"""
#     function translateClassdefElements(
#         inMKAbsynClassPartLst::List{<:MKAbsyn.ClassPart},
#     )::List{SCode.Element}
#         local outElementLst::List{SCode.Element}

#         outElementLst = begin
#             local els::List{SCode.Element}
#             local es_1::List{SCode.Element}
#             local els_1::List{SCode.Element}
#             local es::List{MKAbsyn.ElementItem}
#             local rest::List{MKAbsyn.ClassPart}
#             @match inMKAbsynClassPartLst begin
#                 nil() => begin
#                     nil
#                 end

#                 MKAbsyn.PUBLIC(contents=es) <| rest => begin
#                     es_1 = translateEitemlist(es, SCode.PUBLIC())
#                     els = translateClassdefElements(rest)
#                     els = listAppend(es_1, els)
#                     els
#                 end

#                 MKAbsyn.PROTECTED(contents=es) <| rest => begin
#                     es_1 = translateEitemlist(es, SCode.PROTECTED())
#                     els = translateClassdefElements(rest)
#                     els = listAppend(es_1, els)
#                     els
#                 end

#                 _ <| rest => begin
#                     translateClassdefElements(rest)
#                 end
#             end
#         end
#         #= /* ignore all other than PUBLIC and PROTECTED, i.e. elements */ =#
#         return outElementLst
#     end

#     """ #= Convert an MKAbsyn.ClassPart list to an Equation list. =#"""
#     function translateClassdefEquations(
#         inMKAbsynClassPartLst::List{<:MKAbsyn.ClassPart},
#     )::List{SCode.Equation}
#         local outEquationLst::List{SCode.Equation}

#         outEquationLst = begin
#             local eqs::List{SCode.Equation}
#             local eql_1::List{SCode.Equation}
#             local eqs_1::List{SCode.Equation}
#             local eql::List{MKAbsyn.EquationItem}
#             local rest::List{MKAbsyn.ClassPart}
#             @match inMKAbsynClassPartLst begin
#                 nil() => begin
#                     nil
#                 end

#                 MKAbsyn.EQUATIONS(contents=eql) <| rest => begin
#                     eql_1 = translateEquations(eql, false)
#                     eqs = translateClassdefEquations(rest)
#                     eqs_1 = listAppend(eqs, eql_1)
#                     eqs_1
#                 end

#                 _ <| rest => begin
#                     eqs = translateClassdefEquations(rest)
#                     eqs
#                 end
#             end
#         end
#         #= /* ignore everthing other than equations */ =#
#         return outEquationLst
#     end

#     """ #= Convert an MKAbsyn.ClassPart list to an initial Equation list. =#"""
#     function translateClassdefInitialequations(
#         inMKAbsynClassPartLst::List{<:MKAbsyn.ClassPart},
#     )::List{SCode.Equation}
#         local outEquationLst::List{SCode.Equation}

#         outEquationLst = begin
#             local eqs::List{SCode.Equation}
#             local eql_1::List{SCode.Equation}
#             local eqs_1::List{SCode.Equation}
#             local eql::List{MKAbsyn.EquationItem}
#             local rest::List{MKAbsyn.ClassPart}
#             @match inMKAbsynClassPartLst begin
#                 nil() => begin
#                     nil
#                 end

#                 MKAbsyn.INITIALEQUATIONS(contents=eql) <| rest => begin
#                     eql_1 = translateEquations(eql, true)
#                     eqs = translateClassdefInitialequations(rest)
#                     eqs_1 = listAppend(eqs, eql_1)
#                     eqs_1
#                 end

#                 _ <| rest => begin
#                     eqs = translateClassdefInitialequations(rest)
#                     eqs
#                 end
#             end
#         end
#         #= /* ignore everthing other than equations */ =#
#         return outEquationLst
#     end

#     """ #= Convert an MKAbsyn.ClassPart list to an Algorithm list. =#"""
#     function translateClassdefAlgorithms(
#         inMKAbsynClassPartLst::List{<:MKAbsyn.ClassPart},
#     )::List{SCode.AlgorithmSection}
#         local outAlgorithmLst::List{SCode.AlgorithmSection}

#         outAlgorithmLst = begin
#             local als::List{SCode.AlgorithmSection}
#             local als_1::List{SCode.AlgorithmSection}
#             local al_1::List{SCode.Statement}
#             local al::List{MKAbsyn.AlgorithmItem}
#             local rest::List{MKAbsyn.ClassPart}
#             local cp::MKAbsyn.ClassPart
#             @match inMKAbsynClassPartLst begin
#                 nil() => begin
#                     nil
#                 end

#                 MKAbsyn.ALGORITHMS(contents=al) <| rest => begin
#                     al_1 = translateClassdefAlgorithmitems(al)
#                     als = translateClassdefAlgorithms(rest)
#                     als_1 = _cons(SCode.ALGORITHM(al_1), als)
#                     als_1
#                 end

#                 cp <| rest => begin
#                     als = translateClassdefAlgorithms(rest)
#                     als
#                 end

#                 _ => begin
#                     fail()
#                 end
#             end
#         end
#         #= /* ignore everthing other than algorithms */ =#
#         return outAlgorithmLst
#     end

#     """ #= Convert an MKAbsyn.ClassPart list to an Constraint list. =#"""
#     function translateClassdefConstraints(
#         inMKAbsynClassPartLst::List{<:MKAbsyn.ClassPart},
#     )::List{SCode.ConstraintSection}
#         local outConstraintLst::List{SCode.ConstraintSection}

#         outConstraintLst = begin
#             local cos::List{SCode.ConstraintSection}
#             local cos_1::List{SCode.ConstraintSection}
#             local consts::List{MKAbsyn.Exp}
#             local rest::List{MKAbsyn.ClassPart}
#             local cp::MKAbsyn.ClassPart
#             @match inMKAbsynClassPartLst begin
#                 nil() => begin
#                     nil
#                 end

#                 MKAbsyn.CONSTRAINTS(contents=consts) <| rest => begin
#                     cos = translateClassdefConstraints(rest)
#                     cos_1 = _cons(SCode.CONSTRAINTS(consts), cos)
#                     cos_1
#                 end

#                 cp <| rest => begin
#                     cos = translateClassdefConstraints(rest)
#                     cos
#                 end

#                 _ => begin
#                     fail()
#                 end
#             end
#         end
#         #= /* ignore everthing other than Constraints */ =#
#         return outConstraintLst
#     end

#     """ #= Convert an MKAbsyn.ClassPart list to an initial Algorithm list. =#"""
#     function translateClassdefInitialalgorithms(
#         inMKAbsynClassPartLst::List{<:MKAbsyn.ClassPart},
#     )::List{SCode.AlgorithmSection}
#         local outAlgorithmLst::List{SCode.AlgorithmSection}

#         outAlgorithmLst = begin
#             local als::List{SCode.AlgorithmSection}
#             local als_1::List{SCode.AlgorithmSection}
#             local stmts::List{SCode.Statement}
#             local al::List{MKAbsyn.AlgorithmItem}
#             local rest::List{MKAbsyn.ClassPart}
#             @match inMKAbsynClassPartLst begin
#                 nil() => begin
#                     nil
#                 end

#                 MKAbsyn.INITIALALGORITHMS(contents=al) <| rest => begin
#                     stmts = translateClassdefAlgorithmitems(al)
#                     als = translateClassdefInitialalgorithms(rest)
#                     als_1 = _cons(SCode.ALGORITHM(stmts), als)
#                     als_1
#                 end

#                 _ <| rest => begin
#                     als = translateClassdefInitialalgorithms(rest)
#                     als
#                 end
#             end
#         end
#         #= /* ignore everthing other than algorithms */ =#
#         return outAlgorithmLst
#     end

#     function translateClassdefAlgorithmitems(
#         inStatements::List{<:MKAbsyn.AlgorithmItem},
#     )::List{SCode.Statement}
#         local outStatements::List{SCode.Statement}

#         outStatements = list(
#             translateClassdefAlgorithmItem(stmt)
#             for stmt in inStatements if MKAbsynUtil.isAlgorithmItem(stmt)
#         )
#         return outStatements
#     end

#     """ #= Translates an MKAbsyn algorithm (statement) into SCode statement. =#"""
#     function translateClassdefAlgorithmItem(inAlgorithm::MKAbsyn.AlgorithmItem)::SCode.Statement
#         local outStatement::SCode.Statement

#         local mkAbsynComment::Option{MKAbsyn.Comment}
#         local comment::SCode.Comment
#         local info::SourceInfo
#         local alg::MKAbsyn.Algorithm

#         @match MKAbsyn.ALGORITHMITEM(algorithm_=alg, comment=mkAbsynComment, info=info) =
#             inAlgorithm
#         (comment, info) = translateCommentWithLineInfoChanges(mkAbsynComment, info)
#         outStatement = begin
#             local body::List{SCode.Statement}
#             local else_body::List{SCode.Statement}
#             local branches::List{Tuple{MKAbsyn.Exp,List{SCode.Statement}}}
#             local iter_name::String
#             local iter_range::Option{MKAbsyn.Exp}
#             local stmt::SCode.Statement
#             local e1::MKAbsyn.Exp
#             local e2::MKAbsyn.Exp
#             local e3::MKAbsyn.Exp
#             local cr::MKAbsyn.ComponentRef
#             @match alg begin
#                 MKAbsyn.ALG_ASSIGN(__) => begin
#                     SCode.ALG_ASSIGN(alg.assignComponent, alg.value, comment, info)
#                 end

#                 MKAbsyn.ALG_IF(__) => begin
#                     body = translateClassdefAlgorithmitems(alg.trueBranch)
#                     else_body = translateClassdefAlgorithmitems(alg.elseBranch)
#                     branches = translateAlgBranches(alg.elseIfAlgorithmBranch)
#                     SCode.ALG_IF(alg.ifExp, body, branches, else_body, comment, info)
#                 end

#                 MKAbsyn.ALG_FOR(__) => begin
#                     body = translateClassdefAlgorithmitems(alg.forBody)
#                     #=  Convert for-loops with multiple iterators into nested for-loops.
#                     =#
#                     for i in listReverse(alg.iterators)
#                         (iter_name, iter_range) = translateIterator(i, info)
#                         body = list(SCode.ALG_FOR(iter_name, iter_range, body, comment, info))
#                     end
#                     listHead(body)
#                 end

#                 MKAbsyn.ALG_PARFOR(__) => begin
#                     body = translateClassdefAlgorithmitems(alg.parforBody)
#                     #=  Convert for-loops with multiple iterators into nested for-loops.
#                     =#
#                     for i in listReverse(alg.iterators)
#                         (iter_name, iter_range) = translateIterator(i, info)
#                         body =
#                             list(SCode.ALG_PARFOR(iter_name, iter_range, body, comment, info))
#                     end
#                     listHead(body)
#                 end

#                 MKAbsyn.ALG_WHILE(__) => begin
#                     body = translateClassdefAlgorithmitems(alg.whileBody)
#                     SCode.ALG_WHILE(alg.boolExpr, body, comment, info)
#                 end

#                 MKAbsyn.ALG_WHEN_A(__) => begin
#                     branches = translateAlgBranches(_cons(
#                         (alg.boolExpr, alg.whenBody),
#                         alg.elseWhenAlgorithmBranch,
#                     ))
#                     SCode.ALG_WHEN_A(branches, comment, info)
#                 end

#                 MKAbsyn.ALG_NORETCALL(
#                     functionCall=MKAbsyn.CREF_IDENT(name="assert"),
#                     functionArgs=MKAbsyn.FUNCTIONARGS(args=e1 <| e2 <| nil(), argNames=nil()),
#                 ) => begin
#                     SCode.ALG_ASSERT(e1, e2, ASSERTION_LEVEL_ERROR, comment, info)
#                 end

#                 MKAbsyn.ALG_NORETCALL(
#                     functionCall=MKAbsyn.CREF_IDENT(name="assert"),
#                     functionArgs=MKAbsyn.FUNCTIONARGS(args=e1 <| e2 <| e3 <| nil(), argNames=nil()),
#                 ) => begin
#                     SCode.ALG_ASSERT(e1, e2, e3, comment, info)
#                 end

#                 MKAbsyn.ALG_NORETCALL(
#                     functionCall=MKAbsyn.CREF_IDENT(name="assert"),
#                     functionArgs=MKAbsyn.FUNCTIONARGS(
#                         args=e1 <| e2 <| nil(),
#                         argNames=MKAbsyn.NAMEDARG("level", e3) <| nil(),
#                     ),
#                 ) => begin
#                     SCode.ALG_ASSERT(e1, e2, e3, comment, info)
#                 end

#                 MKAbsyn.ALG_NORETCALL(
#                     functionCall=MKAbsyn.CREF_IDENT(name="terminate"),
#                     functionArgs=MKAbsyn.FUNCTIONARGS(args=e1 <| nil(), argNames=nil()),
#                 ) => begin
#                     SCode.ALG_TERMINATE(e1, comment, info)
#                 end

#                 MKAbsyn.ALG_NORETCALL(
#                     functionCall=MKAbsyn.CREF_IDENT(name="reinit"),
#                     functionArgs=MKAbsyn.FUNCTIONARGS(args=e1 <| e2 <| nil(), argNames=nil()),
#                 ) => begin
#                     SCode.ALG_REINIT(e1, e2, comment, info)
#                 end

#                 MKAbsyn.ALG_NORETCALL(__) => begin
#                     #=  assert(condition, message)
#                     =#
#                     #=  assert(condition, message, level)
#                     =#
#                     #=  assert(condition, message, level = arg)
#                     =#
#                     e1 = MKAbsyn.CALL(alg.functionCall, alg.functionArgs, nil)
#                     SCode.ALG_NORETCALL(e1, comment, info)
#                 end

#                 MKAbsyn.ALG_FAILURE(__) => begin
#                     body = translateClassdefAlgorithmitems(alg.equ)
#                     SCode.ALG_FAILURE(body, comment, info)
#                 end

#                 MKAbsyn.ALG_TRY(__) => begin
#                     body = translateClassdefAlgorithmitems(alg.body)
#                     else_body = translateClassdefAlgorithmitems(alg.elseBody)
#                     SCode.ALG_TRY(body, else_body, comment, info)
#                 end

#                 MKAbsyn.ALG_RETURN(__) => begin
#                     SCode.ALG_RETURN(comment, info)
#                 end

#                 MKAbsyn.ALG_BREAK(__) => begin
#                     SCode.ALG_BREAK(comment, info)
#                 end

#                 MKAbsyn.ALG_CONTINUE(__) => begin
#                     SCode.ALG_CONTINUE(comment, info)
#                 end
#             end
#         end
#         return outStatement
#     end

#     """ #= Translates the elseif or elsewhen branches from MKAbsyn to SCode form. =#"""
#     function translateAlgBranches(
#         inBranches::List{<:Tuple{<:MKAbsyn.Exp,List{<:MKAbsyn.AlgorithmItem}}},
#     )::List{Tuple{MKAbsyn.Exp,List{SCode.Statement}}}
#         local outBranches::List{Tuple{MKAbsyn.Exp,List{SCode.Statement}}}

#         local condition::MKAbsyn.Exp
#         local body::List{MKAbsyn.AlgorithmItem}

#         outBranches = list(
#             begin
#                 @match branch begin
#                     (condition, body) => begin
#                         (condition, translateClassdefAlgorithmitems(body))
#                     end
#                 end
#             end for branch in inBranches
#         )
#         return outBranches
#     end

#     """ #= Converts an MKAbsyn.ClassPart list to an SCode.ExternalDecl option.
#     The list should only contain one external declaration, so pick the first one. =#"""
#     function translateClassdefExternaldecls(
#         inMKAbsynClassPartLst::List{<:MKAbsyn.ClassPart},
#     )::Option{SCode.ExternalDecl}
#         local outMKAbsynExternalDeclOption::Option{SCode.ExternalDecl}

#         outMKAbsynExternalDeclOption = begin
#             local res::Option{SCode.ExternalDecl}
#             local rest::List{MKAbsyn.ClassPart}
#             local fn_name::Option{SCode.Ident}
#             local lang::Option{String}
#             local output_::Option{MKAbsyn.ComponentRef}
#             local args::List{MKAbsyn.Exp}
#             local aann::Option{MKAbsyn.Annotation}
#             local sann::Option{SCode.Annotation}
#             @match inMKAbsynClassPartLst begin
#                 MKAbsyn.EXTERNAL(externalDecl=MKAbsyn.EXTERNALDECL(fn_name, lang, output_, args, aann)) <| _ => begin
#                     sann = translateAnnotationOpt(aann)
#                     SOME(SCode.EXTERNALDECL(fn_name, lang, output_, args, sann))
#                 end

#                 _ <| rest => begin
#                     res = translateClassdefExternaldecls(rest)
#                     res
#                 end

#                 nil() => begin
#                     NONE()
#                 end
#             end
#         end
#         return outMKAbsynExternalDeclOption
#     end

#     """ #= This function converts a list of MKAbsyn.ElementItem to a list of SCode.Element.
#     The boolean argument flags whether the elements are protected.
#     Annotations are not translated, i.e. they are removed when converting to SCode. =#"""
#     function translateEitemlist(
#         inMKAbsynElementItemLst::List{<:MKAbsyn.ElementItem},
#         inVisibility::SCode.Visibility,
#     )::List{SCode.Element}
#         local outElementLst::List{SCode.Element}

#         local l::List{SCode.Element} = nil
#         local es::List{MKAbsyn.ElementItem} = inMKAbsynElementItemLst
#         local ei::MKAbsyn.ElementItem
#         local vis::SCode.Visibility
#         local e::MKAbsyn.Element

#         for ei in es
#             _ = begin
#                 local e_1::List{SCode.Element}
#                 @match ei begin
#                     MKAbsyn.ELEMENTITEM(element=e) => begin
#                         e_1 = translateElement(e, inVisibility)
#                         l = ListUtil.append_reverse(e_1, l)
#                         ()
#                     end

#                     _ => begin
#                         ()
#                     end
#                 end
#             end
#         end

#         outElementLst = listReverse(l)
#         return outElementLst
#     end

#     #=  stefan
#     =#

#     """ #= translates an MKAbsyn.Annotation into an SCode.Annotation =#"""
#     function translateAnnotation(inAnnotation::MKAbsyn.Annotation)::Option{SCode.Annotation}
#         local outAnnotation::Option{SCode.Annotation}

#         outAnnotation = begin
#             local args::List{MKAbsyn.ElementArg}
#             local m::SCode.Mod
#             @match inAnnotation begin
#                 MKAbsyn.ANNOTATION(elementArgs=nil()) => begin
#                     NONE()
#                 end

#                 MKAbsyn.ANNOTATION(elementArgs=args) => begin
#                     m = translateMod(
#                         SOME(MKAbsyn.CLASSMOD(args, MKAbsyn.NOMOD())),
#                         SCode.NOT_FINAL(),
#                         SCode.NOT_EACH(),
#                         MKAbsynUtil.dummyInfo,
#                     )
#                     if SCodeUtil.isEmptyMod(m)
#                         NONE()
#                     else
#                         SOME(SCode.ANNOTATION(m))
#                     end
#                 end
#             end
#         end
#         return outAnnotation
#     end

#     function translateAnnotationOpt(
#         mkAbsynAnnotation::Option{<:MKAbsyn.Annotation},
#     )::Option{SCode.Annotation}
#         local scodeAnnotation::Option{SCode.Annotation}

#         scodeAnnotation = begin
#             local ann::MKAbsyn.Annotation
#             @match mkAbsynAnnotation begin
#                 SOME(ann) => begin
#                     translateAnnotation(ann)
#                 end

#                 _ => begin
#                     NONE()
#                 end
#             end
#         end
#         return scodeAnnotation
#     end

#     """ #= This function converts an MKAbsyn.Element to a list of SCode.Element.
#     The original element may declare several components at once, and
#     those are separated to several declarations in the result. =#"""
#     function translateElement(
#         inElement::MKAbsyn.Element,
#         inVisibility::SCode.Visibility,
#     )::List{SCode.Element}
#         local outElementLst::List{SCode.Element}

#         outElementLst = begin
#             local es::List{SCode.Element}
#             local f::Bool
#             local repl::Option{MKAbsyn.RedeclareKeywords}
#             local s::MKAbsyn.ElementSpec
#             local io::MKAbsyn.InnerOuter
#             local info::SourceInfo
#             local cc::Option{MKAbsyn.ConstrainClass}
#             local expOpt::Option{String}
#             local weightOpt::Option{AbstractFloat}
#             local args::List{MKAbsyn.NamedArg}
#             local name::String
#             local vis::SCode.Visibility
#             @match (inElement, inVisibility) begin
#                 (
#                     MKAbsyn.ELEMENT(
#                         constrainClass=cc,
#                         finalPrefix=f,
#                         innerOuter=io,
#                         redeclareKeywords=repl,
#                         specification=s,
#                         info=info,
#                     ),
#                     vis,
#                 ) => begin
#                     es = translateElementspec(cc, f, io, repl, vis, s, info)
#                     es
#                 end

#                 (MKAbsyn.DEFINEUNIT(name, args, info), vis) => begin
#                     expOpt = translateDefineunitParam(args, "exp")
#                     weightOpt = translateDefineunitParam2(args, "weight")
#                     list(SCode.DEFINEUNIT(name, vis, expOpt, weightOpt, info))
#                 end
#                 _ => fail()
#             end
#         end
#         return outElementLst
#     end

#     """ #=  help function to translateElement =#"""
#     function translateDefineunitParam(
#         inArgs::List{<:MKAbsyn.NamedArg},
#         inArg::String,
#     )::Option{String}
#         local expOpt::Option{String}

#         expOpt = begin
#             local str::String
#             local name::String
#             local arg::String
#             local args::List{MKAbsyn.NamedArg}
#             @match (inArgs, inArg) begin
#                 (MKAbsyn.NAMEDARG(name, MKAbsyn.STRING(str)) <| _, arg) where {(name == arg)} => begin
#                     SOME(str)
#                 end

#                 (nil(), _) => begin
#                     NONE()
#                 end

#                 (_ <| args, arg) => begin
#                     translateDefineunitParam(args, arg)
#                 end
#             end
#         end
#         return expOpt
#     end

#     """ #=  help function to translateElement =#"""
#     function translateDefineunitParam2(
#         inArgs::List{<:MKAbsyn.NamedArg},
#         inArg::String,
#     )::Option{AbstractFloat}
#         local weightOpt::Option{AbstractFloat}

#         weightOpt = begin
#             local name::String
#             local arg::String
#             local s::String
#             local r::AbstractFloat
#             local args::List{MKAbsyn.NamedArg}
#             @match (inArgs, inArg) begin
#                 (MKAbsyn.NAMEDARG(name, MKAbsyn.REAL(s)) <| _, arg) where {(name == arg)} => begin
#                     r = System.stringReal(s)
#                     SOME(r)
#                 end

#                 (nil(), _) => begin
#                     NONE()
#                 end

#                 (_ <| args, arg) => begin
#                     translateDefineunitParam2(args, arg)
#                 end
#             end
#         end
#         return weightOpt
#     end

#     """
#         This function turns an MKAbsyn.ElementSpec to a list of SCode.Element.
#         The boolean arguments say if the element is final and protected, respectively.
#     """
#     function translateElementspec(
#         cc::Option{<:MKAbsyn.ConstrainClass},
#         finalPrefix::Bool,
#         io::MKAbsyn.InnerOuter,
#         inRedeclareKeywords::Option{<:MKAbsyn.RedeclareKeywords},
#         inVisibility::SCode.Visibility,
#         inElementSpec4::MKAbsyn.ElementSpec,
#         inInfo::SourceInfo,
#     )::List{SCode.Element}
#         local outElementLst::List{SCode.Element}

#         outElementLst = begin
#             local absann::MKAbsyn.Annotation
#             local ad::List{SCode.Subscript}
#             local ann::Option{SCode.Annotation}
#             local args::List{MKAbsyn.ElementArg}
#             local attr::MKAbsyn.ElementAttributes
#             local cl::MKAbsyn.Class
#             local cls::SCode.Element
#             local cmt::SCode.Comment
#             local comment::Option{MKAbsyn.Comment}
#             local cond::Option{MKAbsyn.Exp}
#             local ct::SCode.ConnectorType
#             local d::List{SCode.Subscript}
#             local de::MKAbsyn.ClassDef
#             local de_1::SCode.ClassDef
#             local di::MKAbsyn.Direction
#             local e::Bool
#             local fi::Bool
#             local fl::Bool
#             local i::SourceInfo
#             local imp::MKAbsyn.Import
#             local info::SourceInfo
#             local isf::MKAbsyn.IsField
#             local m::Option{MKAbsyn.Modification}
#             local mod::SCode.Mod
#             local md::Bool
#             local n::String
#             local pa::Bool
#             local parallelism::MKAbsyn.Parallelism
#             local path::MKAbsyn.Path
#             local prefixes::SCode.Prefixes
#             local prl1::SCode.Parallelism
#             local re::MKAbsyn.Restriction
#             local re_1::SCode.Restriction
#             local redecl::Bool
#             local repl::Option{MKAbsyn.RedeclareKeywords}
#             local repl_1::Bool
#             local rp::Bool
#             local sEnc::SCode.Encapsulated
#             local sFin::SCode.Final
#             local sPar::SCode.Partial
#             local sRed::SCode.Redeclare
#             local sRep::SCode.Replaceable
#             local scc::Option{SCode.ConstrainClass}
#             local st::Bool
#             local t::MKAbsyn.TypeSpec
#             local tot_dim::List{SCode.Subscript}
#             local var1::SCode.Variability
#             local variability::MKAbsyn.Variability
#             local vis::SCode.Visibility
#             local xs::List{MKAbsyn.ComponentItem}
#             local xs_1::List{SCode.Element}
#             @match (cc, finalPrefix, io, inRedeclareKeywords, inVisibility, inElementSpec4, inInfo) begin
#                 (
#                     _,
#                     _,
#                     _,
#                     repl,
#                     vis,
#                     MKAbsyn.CLASSDEF(
#                         replaceable_=rp,
#                         class_=MKAbsyn.CLASS(
#                             name=n,
#                             partialPrefix=pa,
#                             encapsulatedPrefix=e,
#                             restriction=MKAbsyn.R_OPERATOR(__),
#                             body=de,
#                             info=i,
#                         ),
#                     ),
#                     _,
#                 ) => begin
#                     (de_1, cmt) = translateOperatorDef(de, n, i)
#                     (_, redecl) = translateRedeclarekeywords(repl)
#                     sRed = SCodeUtil.boolRedeclare(redecl)
#                     sFin = SCodeUtil.boolFinal(finalPrefix)
#                     scc = translateConstrainClass(cc)
#                     sRep = if rp
#                         SCode.REPLACEABLE(scc)
#                     else
#                         SCode.NOT_REPLACEABLE()
#                     end
#                     sEnc = SCodeUtil.boolEncapsulated(e)
#                     sPar = SCodeUtil.boolPartial(pa)
#                     cls = SCode.CLASS(
#                         n,
#                         SCode.PREFIXES(vis, sRed, sFin, io, sRep),
#                         sEnc,
#                         sPar,
#                         SCode.R_OPERATOR(),
#                         de_1,
#                         cmt,
#                         i,
#                     )
#                     list(cls)
#                 end

#                 (
#                     _,
#                     _,
#                     _,
#                     repl,
#                     vis,
#                     MKAbsyn.CLASSDEF(
#                         replaceable_=rp,
#                         class_=cl && MKAbsyn.CLASS(
#                             name=n,
#                             partialPrefix=pa,
#                             encapsulatedPrefix=e,
#                             restriction=re,
#                             body=de,
#                             info=i,
#                         ),
#                     ),
#                     _,
#                 ) => begin
#                     re_1 = translateRestriction(cl, re)
#                     (de_1, cmt) = translateClassdef(de, i, re_1)
#                     (_, redecl) = translateRedeclarekeywords(repl)
#                     sRed = SCodeUtil.boolRedeclare(redecl)
#                     sFin = SCodeUtil.boolFinal(finalPrefix)
#                     scc = translateConstrainClass(cc)
#                     sRep = if rp
#                         SCode.REPLACEABLE(scc)
#                     else
#                         SCode.NOT_REPLACEABLE()
#                     end
#                     sEnc = SCodeUtil.boolEncapsulated(e)
#                     sPar = SCodeUtil.boolPartial(pa)
#                     cls = SCode.CLASS(
#                         n,
#                         SCode.PREFIXES(vis, sRed, sFin, io, sRep),
#                         sEnc,
#                         sPar,
#                         re_1,
#                         de_1,
#                         cmt,
#                         i,
#                     )
#                     list(cls)
#                 end

#                 (
#                     _,
#                     _,
#                     _,
#                     _,
#                     vis,
#                     MKAbsyn.EXTENDS(path=path, elementArg=args, annotationOpt=NONE()),
#                     info,
#                 ) => begin
#                     mod = translateMod(
#                         SOME(MKAbsyn.CLASSMOD(args, MKAbsyn.NOMOD())),
#                         SCode.NOT_FINAL(),
#                         SCode.NOT_EACH(),
#                         MKAbsynUtil.dummyInfo,
#                     )
#                     list(SCode.EXTENDS(path, vis, mod, NONE(), info))
#                 end

#                 (
#                     _,
#                     _,
#                     _,
#                     _,
#                     vis,
#                     MKAbsyn.EXTENDS(path=path, elementArg=args, annotationOpt=SOME(absann)),
#                     info,
#                 ) => begin
#                     mod = translateMod(
#                         SOME(MKAbsyn.CLASSMOD(args, MKAbsyn.NOMOD())),
#                         SCode.NOT_FINAL(),
#                         SCode.NOT_EACH(),
#                         MKAbsynUtil.dummyInfo,
#                     )
#                     ann = translateAnnotation(absann)
#                     list(SCode.EXTENDS(path, vis, mod, ann, info))
#                 end

#                 (_, _, _, _, _, MKAbsyn.COMPONENTS(components=nil()), _) => begin
#                     nil
#                 end

#                 (
#                     _,
#                     _,
#                     _,
#                     repl,
#                     vis,
#                     MKAbsyn.COMPONENTS(
#                         attributes=MKAbsyn.ATTR(
#                             flowPrefix=fl,
#                             streamPrefix=st,
#                             parallelism=parallelism,
#                             variability=variability,
#                             direction=di,
#                             isField=isf,
#                             arrayDim=ad,
#                             isMode=md
#                         ),
#                         typeSpec=t,
#                     ),
#                     info,
#                 ) => begin
#                     xs_1 = nil
#                     for comp in inElementSpec4.components
#                         @match MKAbsyn.COMPONENTITEM(
#                             component=MKAbsyn.COMPONENT(name=n, arrayDim=d, modification=m),
#                             comment=comment,
#                             condition=cond,
#                         ) = comp
#                         checkTypeSpec(t, info)
#                         setHasInnerOuterDefinitionsHandler(io)
#                         setHasStreamConnectorsHandler(st)
#                         mod = translateMod(m, SCode.NOT_FINAL(), SCode.NOT_EACH(), info)
#                         prl1 = translateParallelism(parallelism)
#                         var1 = translateVariability(variability)
#                         tot_dim = listAppend(d, ad)
#                         (repl_1, redecl) = translateRedeclarekeywords(repl)
#                         (cmt, info) = translateCommentWithLineInfoChanges(comment, info)
#                         sFin = SCodeUtil.boolFinal(finalPrefix)
#                         sRed = SCodeUtil.boolRedeclare(redecl)
#                         scc = translateConstrainClass(cc)
#                         sRep = if repl_1
#                             SCode.REPLACEABLE(scc)
#                         else
#                             SCode.NOT_REPLACEABLE()
#                         end
#                         ct = translateConnectorType(fl, st)
#                         prefixes = SCode.PREFIXES(vis, sRed, sFin, io, sRep)
#                         xs_1 = begin
#                             #=  TODO: Improve performance by iterating over all elements at once instead of creating a new MKAbsyn.COMPONENTS in each step...
#                             =#
#                             #=  fprintln(Flags.TRANSLATE, \"translating component: \" + n + \" final: \" + SCodeUtil.finalStr(SCodeUtil.boolFinal(finalPrefix)));
#                             =#
#                             #=  signal the external flag that we have inner/outer definitions
#                             =#
#                             #=  signal the external flag that we have stream connectors
#                             =#
#                             #=  PR. This adds the arraydimension that may be specified together with the type of the component.
#                             =#
#                             local attr1::SCode.Attributes
#                             local attr2::SCode.Attributes
#                             local mod2::SCode.Mod
#                             local inName::String
#                             @match di begin
#                                 MKAbsyn.INPUT_OUTPUT(__) => begin
#                                     inName = "in_" + n
#                                     attr1 = SCode.ATTR(tot_dim, ct, prl1, var1, MKAbsyn.INPUT(), isf)
#                                     attr2 = SCode.ATTR(tot_dim, ct, prl1, var1, MKAbsyn.OUTPUT(), isf)
#                                     mod2 = SCode.MOD(
#                                         SCode.FINAL(),
#                                         SCode.NOT_EACH(),
#                                         nil,
#                                         SOME(MKAbsyn.CREF(MKAbsyn.CREF_IDENT(inName, nil))),
#                                         info,
#                                     )
#                                     _cons(
#                                         SCode.COMPONENT(n, prefixes, attr2, t, mod2, cmt, cond, info),
#                                         _cons(
#                                             SCode.COMPONENT(inName, prefixes, attr1, t, mod, cmt, cond, info),
#                                             xs_1,
#                                         ),
#                                     )
#                                 end

#                                 _ => begin
#                                     _cons(
#                                         SCode.COMPONENT(
#                                             n,
#                                             prefixes,
#                                             SCode.ATTR(tot_dim, ct, prl1, var1, di, isf, md),
#                                             t,
#                                             mod,
#                                             cmt,
#                                             cond,
#                                             info,
#                                         ),
#                                         xs_1,
#                                     )
#                                 end
#                             end
#                         end
#                     end
#                     xs_1 = listReverse(xs_1)
#                     xs_1
#                 end

#                 (_, _, _, _, vis, MKAbsyn.IMPORT(import_=imp, info=info), _) => begin
#                     xs_1 = translateImports(imp, vis, info)
#                     xs_1
#                 end

#                 _ => begin
#                     fail()
#                 end
#             end
#         end
#         return outElementLst
#     end

#     """ #= Used to handle group imports, i.e. A.B.C.{x=a,b} =#"""
#     function translateImports(
#         imp::MKAbsyn.Import,
#         visibility::SCode.Visibility,
#         info::SourceInfo,
#     )::List{SCode.Element}
#         local elts::List{SCode.Element}

#         elts = begin
#             local name::String
#             local p::MKAbsyn.Path
#             local groups::List{MKAbsyn.GroupImport}
#             #= /* Maybe these should give warnings? I don't know. See https:trac.modelica.org/Modelica/ticket/955 */ =#
#             @match (imp, visibility, info) begin
#                 (MKAbsyn.NAMED_IMPORT(name, MKAbsyn.FULLYQUALIFIED(p)), _, _) => begin
#                     translateImports(MKAbsyn.NAMED_IMPORT(name, p), visibility, info)
#                 end

#                 (MKAbsyn.QUAL_IMPORT(MKAbsyn.FULLYQUALIFIED(p)), _, _) => begin
#                     translateImports(MKAbsyn.QUAL_IMPORT(p), visibility, info)
#                 end

#                 (MKAbsyn.UNQUAL_IMPORT(MKAbsyn.FULLYQUALIFIED(p)), _, _) => begin
#                     translateImports(MKAbsyn.UNQUAL_IMPORT(p), visibility, info)
#                 end

#                 (MKAbsyn.GROUP_IMPORT(prefix=p, groups=groups), _, _) => begin
#                     ListUtil.map3(groups, translateGroupImport, p, visibility, info)
#                 end

#                 _ => begin
#                     list(SCode.IMPORT(imp, visibility, info))
#                 end
#             end
#         end
#         return elts
#     end

#     """ #= Used to handle group imports, i.e. A.B.C.{x=a,b} =#"""
#     function translateGroupImport(
#         gimp::MKAbsyn.GroupImport,
#         prefix::MKAbsyn.Path,
#         visibility::SCode.Visibility,
#         info::SourceInfo,
#     )::SCode.Element
#         local elt::SCode.Element

#         elt = begin
#             local name::String
#             local rename::String
#             local path::MKAbsyn.Path
#             local vis::SCode.Visibility
#             @match (gimp, prefix, visibility, info) begin
#                 (MKAbsyn.GROUP_IMPORT_NAME(name=name), _, vis, _) => begin
#                     path = MKAbsynUtil.joinPaths(prefix, MKAbsyn.IDENT(name))
#                     SCode.IMPORT(MKAbsyn.QUAL_IMPORT(path), vis, info)
#                 end

#                 (MKAbsyn.GROUP_IMPORT_RENAME(rename=rename, name=name), _, vis, _) => begin
#                     path = MKAbsynUtil.joinPaths(prefix, MKAbsyn.IDENT(name))
#                     SCode.IMPORT(MKAbsyn.NAMED_IMPORT(rename, path), vis, info)
#                 end
#             end
#         end
#         return elt
#     end

#     """ #= @author: adrpo
#     This function will set the external flag that signals
#     that a model has inner/outer component definitions =#"""
#     function setHasInnerOuterDefinitionsHandler(io::MKAbsyn.InnerOuter)
#         _ = begin
#             @match io begin
#                 MKAbsyn.NOT_INNER_OUTER(__) => begin
#                     ()
#                 end

#                 _ => begin
#                     System.setHasInnerOuterDefinitions(true)
#                     ()
#                 end
#             end
#         end
#         #=  no inner outer!
#         =#
#         #=  has inner, outer or innerouter components
#         =#
#     end

#     """ #= @author: adrpo
#     This function will set the external flag that signals
#     that a model has stream connectors =#
#     """
#     function setHasStreamConnectorsHandler(streamPrefix::Bool)
#         _ = begin
#             @match streamPrefix begin
#                 false => begin
#                     ()
#                 end

#                 true => begin
#                     System.setHasStreamConnectors(true)
#                     ()
#                 end
#             end
#         end
#         #=  no stream prefix
#         =#
#         #=  has stream prefix
#         =#
#     end

#     """ #= author: PA
#     For now, translate to bool, replaceable. =#"""
#     function translateRedeclarekeywords(
#         inRedeclKeywords::Option{<:MKAbsyn.RedeclareKeywords},
#     )::Tuple{Bool,Bool}
#         local outIsRedeclared::Bool
#         local outIsReplaceable::Bool

#         (outIsReplaceable, outIsRedeclared) = begin
#             @match inRedeclKeywords begin
#                 SOME(MKAbsyn.REDECLARE(__)) => begin
#                     (false, true)
#                 end

#                 SOME(MKAbsyn.REPLACEABLE(__)) => begin
#                     (true, false)
#                 end

#                 SOME(MKAbsyn.REDECLARE_REPLACEABLE(__)) => begin
#                     (true, true)
#                 end

#                 _ => begin
#                     (false, false)
#                 end
#             end
#         end
#         return (outIsReplaceable, outIsRedeclared)
#     end

#     function translateConstrainClass(
#         inConstrainClass::Option{<:MKAbsyn.ConstrainClass},
#     )::Option{SCode.ConstrainClass}
#         local outConstrainClass::Option{SCode.ConstrainClass}

#         outConstrainClass = begin
#             local cc_path::MKAbsyn.Path
#             local eltargs::List{MKAbsyn.ElementArg}
#             local cmt::Option{MKAbsyn.Comment}
#             local cc_cmt::SCode.Comment
#             local mod::MKAbsyn.Modification
#             local cc_mod::SCode.Mod
#             @match inConstrainClass begin
#                 SOME(MKAbsyn.CONSTRAINCLASS(
#                     elementSpec=MKAbsyn.EXTENDS(path=cc_path, elementArg=eltargs),
#                     comment=cmt,
#                 )) => begin
#                     mod = MKAbsyn.CLASSMOD(eltargs, MKAbsyn.NOMOD())
#                     cc_mod = translateMod(
#                         SOME(mod),
#                         SCode.NOT_FINAL(),
#                         SCode.NOT_EACH(),
#                         MKAbsynUtil.dummyInfo,
#                     )
#                     cc_cmt = translateComment(cmt)
#                     SOME(SCode.CONSTRAINCLASS(cc_path, cc_mod, cc_cmt))
#                 end

#                 _ => begin
#                     NONE()
#                 end
#             end
#         end
#         return outConstrainClass
#     end

#     """ #= Converts an MKAbsyn.Parallelism to SCode.Parallelism. =#"""
#     function translateParallelism(inParallelism::MKAbsyn.Parallelism)::SCode.Parallelism
#         local outParallelism::SCode.Parallelism

#         outParallelism = begin
#             @match inParallelism begin
#                 MKAbsyn.PARGLOBAL(__) => begin
#                     SCode.PARGLOBAL()
#                 end

#                 MKAbsyn.PARLOCAL(__) => begin
#                     SCode.PARLOCAL()
#                 end

#                 MKAbsyn.NON_PARALLEL(__) => begin
#                     SCode.NON_PARALLEL()
#                 end
#             end
#         end
#         return outParallelism
#     end

#     """ #= Converts an MKAbsyn.Variability to SCode.Variability. =#"""
#     function translateVariability(inVariability::MKAbsyn.Variability)::SCode.Variability
#         local outVariability::SCode.Variability

#         outVariability = begin
#             @match inVariability begin
#                 MKAbsyn.VAR(__) => begin
#                     SCode.VAR()
#                 end

#                 MKAbsyn.DISCRETE(__) => begin
#                     SCode.DISCRETE()
#                 end

#                 MKAbsyn.PARAM(__) => begin
#                     SCode.PARAM()
#                 end

#                 MKAbsyn.CONST(__) => begin
#                     SCode.CONST()
#                 end
#             end
#         end
#         return outVariability
#     end

#     """ #= This function transforms a list of MKAbsyn.Equation to a list of
#     SCode.Equation, by applying the translateEquation function to each
#     equation. =#"""
#     function translateEquations(
#         inMKAbsynEquationItemLst::List{<:MKAbsyn.EquationItem},
#         inIsInitial::Bool,
#     )::List{SCode.Equation}
#         local outEquationLst::List{SCode.Equation}

#         outEquationLst = list(
#             begin
#                 local com::SCode.Comment
#                 local info::SourceInfo
#                 @match eq begin
#                     MKAbsyn.EQUATIONITEM(__) => begin
#                         (com, info) = translateCommentWithLineInfoChanges(eq.comment, eq.info)
#                         SCode.EQUATION(translateEquation(eq.equation_, com, info, inIsInitial))
#                     end
#                 end
#             end for eq in inMKAbsynEquationItemLst if begin
#                 @match eq begin
#                     MKAbsyn.EQUATIONITEM(__) => begin
#                         true
#                     end

#                     _ => begin
#                         false
#                     end
#                 end
#             end
#         )
#         return outEquationLst
#     end

#     """ #= Helper function to translateEquations =#"""
#     function translateEEquations(
#         inMKAbsynEquationItemLst::List{<:MKAbsyn.EquationItem},
#         inIsInitial::Bool,
#     )::List{SCode.EEquation}
#         local outEEquationLst::List{SCode.EEquation}

#         outEEquationLst = begin
#             local e_1::SCode.EEquation
#             local es_1::List{SCode.EEquation}
#             local e::MKAbsyn.Equation
#             local es::List{MKAbsyn.EquationItem}
#             local acom::Option{MKAbsyn.Comment}
#             local com::SCode.Comment
#             local info::SourceInfo
#             @match (inMKAbsynEquationItemLst, inIsInitial) begin
#                 (nil(), _) => begin
#                     nil
#                 end

#                 (MKAbsyn.EQUATIONITEM(equation_=e, comment=acom, info=info) <| es, _) => begin
#                     (com, info) = translateCommentWithLineInfoChanges(acom, info)
#                     e_1 = translateEquation(e, com, info, inIsInitial)
#                     es_1 = translateEEquations(es, inIsInitial)
#                     _cons(e_1, es_1)
#                 end

#                 (MKAbsyn.EQUATIONITEMCOMMENT(__) <| es, _) => begin
#                     translateEEquations(es, inIsInitial)
#                 end
#             end
#         end
#         #=  fprintln(Flags.TRANSLATE, \"translating equation: \" + Dump.unparseEquationStr(0, e));
#         =#
#         return outEEquationLst
#     end

#     """ #= turns an MKAbsyn.Comment into an SCode.Comment =#"""
#     function translateCommentWithLineInfoChanges(
#         inComment::Option{<:MKAbsyn.Comment},
#         inInfo::SourceInfo,
#     )::Tuple{SCode.Comment,SourceInfo}
#         local outInfo::SourceInfo
#         local outComment::SCode.Comment

#         outComment = translateComment(inComment)
#         outInfo = getInfoAnnotationOrDefault(outComment, inInfo)
#         return (outComment, outInfo)
#     end

#     """ #= Replaces the file info if there is an annotation __OpenModelica_FileInfo=(\\\"fileName\\\",line). Should be improved. =#"""
#     function getInfoAnnotationOrDefault(comment::SCode.Comment, default::SourceInfo)::SourceInfo
#         local info::SourceInfo

#         info = begin
#             local lst::List{SCode.SubMod}
#             @match (comment, default) begin
#                 (
#                     SCode.COMMENT(
#                         annotation_=SOME(SCode.ANNOTATION(modification=SCode.MOD(subModLst=lst))),
#                     ),
#                     _,
#                 ) => begin
#                     getInfoAnnotationOrDefault2(lst, default)
#                 end

#                 _ => begin
#                     default
#                 end
#             end
#         end
#         return info
#     end

#     function getInfoAnnotationOrDefault2(
#         lst::List{<:SCode.SubMod},
#         default::SourceInfo,
#     )::SourceInfo
#         local info::SourceInfo

#         info = begin
#             local rest::List{SCode.SubMod}
#             local fileName::String
#             local line::Integer
#             @match (lst, default) begin
#                 (nil(), _) => begin
#                     default
#                 end

#                 (
#                     SCode.NAMEMOD(
#                         ident="__OpenModelica_FileInfo",
#                         mod=SCode.MOD(
#                             binding=SOME(MKAbsyn.TUPLE(
#                                 MKAbsyn.STRING(fileName) <| MKAbsyn.INTEGER(line) <| nil(),
#                             )),
#                         ),
#                     ) <| _,
#                     _,
#                 ) => begin
#                     SOURCEINFO(fileName, false, line, 0, line, 0, 0.0)
#                 end

#                 (_ <| rest, _) => begin
#                     getInfoAnnotationOrDefault2(rest, default)
#                 end
#             end
#         end
#         return info
#     end

#     """ #= turns an MKAbsyn.Comment into an SCode.Comment =#"""
#     function translateComment(inComment::Option{<:MKAbsyn.Comment})::SCode.Comment
#         local outComment::SCode.Comment

#         outComment = begin
#             local absann::Option{MKAbsyn.Annotation}
#             local ann::Option{SCode.Annotation}
#             local ostr::Option{String}
#             @match inComment begin
#                 NONE() => begin
#                     SCode.noComment
#                 end

#                 SOME(MKAbsyn.COMMENT(absann, ostr)) => begin
#                     ann = translateAnnotationOpt(absann)
#                     ostr = Util.applyOption(ostr, System.unescapedString)
#                     SCode.COMMENT(ann, ostr)
#                 end
#             end
#         end
#         return outComment
#     end

#     """ #= turns an MKAbsyn.Comment into an SCode.Comment =#"""
#     function translateCommentList(
#         inAnns::List{<:MKAbsyn.Annotation},
#         inString::Option{<:String},
#     )::SCode.Comment
#         local outComment::SCode.Comment

#         outComment = begin
#             local absann::MKAbsyn.Annotation
#             local anns::List{MKAbsyn.Annotation}
#             local ann::Option{SCode.Annotation}
#             local ostr::Option{String}
#             @match (inAnns, inString) begin
#                 (nil(), _) => begin
#                     SCode.COMMENT(NONE(), inString)
#                 end

#                 (absann <| nil(), _) => begin
#                     ann = translateAnnotation(absann)
#                     ostr = Util.applyOption(inString, System.unescapedString)
#                     SCode.COMMENT(ann, ostr)
#                 end

#                 (absann <| anns, _) => begin
#                     absann = ListUtil.fold(anns, MKAbsynUtil.mergeAnnotations, absann)
#                     ann = translateAnnotation(absann)
#                     ostr = Util.applyOption(inString, System.unescapedString)
#                     SCode.COMMENT(ann, ostr)
#                 end
#             end
#         end
#         return outComment
#     end

#     """ #= turns an MKAbsyn.Comment into an SCode.Annotation + string =#"""
#     function translateCommentSeparate(
#         inComment::Option{<:MKAbsyn.Comment},
#     )::Tuple{Option{SCode.Annotation},Option{String}}
#         local outStr::Option{String}
#         local outAnn::Option{SCode.Annotation}

#         (outAnn, outStr) = begin
#             local absann::MKAbsyn.Annotation
#             local ann::Option{SCode.Annotation}
#             local str::String
#             @match inComment begin
#                 NONE() => begin
#                     (NONE(), NONE())
#                 end

#                 SOME(MKAbsyn.COMMENT(NONE(), NONE())) => begin
#                     (NONE(), NONE())
#                 end

#                 SOME(MKAbsyn.COMMENT(NONE(), SOME(str))) => begin
#                     (NONE(), SOME(str))
#                 end

#                 SOME(MKAbsyn.COMMENT(SOME(absann), NONE())) => begin
#                     ann = translateAnnotation(absann)
#                     (ann, NONE())
#                 end

#                 SOME(MKAbsyn.COMMENT(SOME(absann), SOME(str))) => begin
#                     ann = translateAnnotation(absann)
#                     (ann, SOME(str))
#                 end
#             end
#         end
#         return (outAnn, outStr)
#     end

#     function translateEquation(
#         inEquation::MKAbsyn.Equation,
#         inComment::SCode.Comment,
#         inInfo::SourceInfo,
#         inIsInitial::Bool,
#     )::SCode.EEquation
#         local outEEquation::SCode.EEquation

#         outEEquation = begin
#             local exp::MKAbsyn.Exp
#             local e1::MKAbsyn.Exp
#             local e2::MKAbsyn.Exp
#             local e3::MKAbsyn.Exp
#             local abody::List{MKAbsyn.Equation}
#             local else_branch::List{SCode.EEquation}
#             local body::List{SCode.EEquation}
#             local branches::List{Tuple{MKAbsyn.Exp,List{SCode.EEquation}}}
#             local iter_name::String
#             local iter_range::Option{MKAbsyn.Exp}
#             local eq::SCode.EEquation
#             local conditions::List{MKAbsyn.Exp}
#             local bodies::List{List{SCode.EEquation}}
#             local cr::MKAbsyn.ComponentRef
#             @match inEquation begin
#                 MKAbsyn.EQ_IF(__) => begin
#                     body = translateEEquations(inEquation.equationTrueItems, inIsInitial)
#                     (conditions, bodies) =
#                         ListUtil.map1_2(inEquation.elseIfBranches, translateEqBranch, inIsInitial)
#                     conditions = _cons(inEquation.ifExp, conditions)
#                     else_branch =
#                         translateEEquations(inEquation.equationElseItems, inIsInitial)
#                     SCode.EQ_IF(conditions, _cons(body, bodies), else_branch, inComment, inInfo)
#                 end

#                 MKAbsyn.EQ_WHEN_E(__) => begin
#                     body = translateEEquations(inEquation.whenEquations, inIsInitial)
#                     (conditions, bodies) =
#                         ListUtil.map1_2(inEquation.elseWhenEquations, translateEqBranch, inIsInitial)
#                     branches = list(@do_threaded_for (c, b) (c, b) (conditions, bodies))
#                     SCode.EQ_WHEN(inEquation.whenExp, body, branches, inComment, inInfo)
#                 end

#                 MKAbsyn.EQ_EQUALS(__) => begin
#                     SCode.EQ_EQUALS(inEquation.leftSide, inEquation.rightSide, inComment, inInfo)
#                 end

#                 MKAbsyn.EQ_PDE(__) => begin
#                     SCode.EQ_PDE(
#                         inEquation.leftSide,
#                         inEquation.rightSide,
#                         inEquation.domain,
#                         inComment,
#                         inInfo,
#                     )
#                 end

#                 MKAbsyn.EQ_CONNECT(__) => begin
#                     if inIsInitial
#                     end
#                     SCode.EQ_CONNECT(inEquation.connector1, inEquation.connector2, inComment, inInfo)
#                 end

#                 MKAbsyn.EQ_FOR(__) => begin
#                     body = translateEEquations(inEquation.forEquations, inIsInitial)
#                     #=  Convert for-loops with multiple iterators into nested for-loops.
#                     =#
#                     for i in listReverse(inEquation.iterators)
#                         (iter_name, iter_range) = translateIterator(i, inInfo)
#                         body =
#                             list(SCode.EQ_FOR(iter_name, iter_range, body, inComment, inInfo))
#                     end
#                     listHead(body)
#                 end

#                 MKAbsyn.EQ_NORETCALL(
#                     functionName=MKAbsyn.CREF_IDENT(name="assert"),
#                     functionArgs=MKAbsyn.FUNCTIONARGS(args=e1 <| e2 <| nil(), argNames=nil()),
#                 ) => begin
#                     SCode.EQ_ASSERT(e1, e2, ASSERTION_LEVEL_ERROR, inComment, inInfo)
#                 end

#                 MKAbsyn.EQ_NORETCALL(
#                     functionName=MKAbsyn.CREF_IDENT(name="assert"),
#                     functionArgs=MKAbsyn.FUNCTIONARGS(args=e1 <| e2 <| e3 <| nil(), argNames=nil()),
#                 ) => begin
#                     SCode.EQ_ASSERT(e1, e2, e3, inComment, inInfo)
#                 end

#                 MKAbsyn.EQ_NORETCALL(
#                     functionName=MKAbsyn.CREF_IDENT(name="assert"),
#                     functionArgs=MKAbsyn.FUNCTIONARGS(
#                         args=e1 <| e2 <| nil(),
#                         argNames=MKAbsyn.NAMEDARG("level", e3) <| nil(),
#                     ),
#                 ) => begin
#                     SCode.EQ_ASSERT(e1, e2, e3, inComment, inInfo)
#                 end

#                 MKAbsyn.EQ_NORETCALL(
#                     functionName=MKAbsyn.CREF_IDENT(name="terminate"),
#                     functionArgs=MKAbsyn.FUNCTIONARGS(args=e1 <| nil(), argNames=nil()),
#                 ) => begin
#                     SCode.EQ_TERMINATE(e1, inComment, inInfo)
#                 end

#                 MKAbsyn.EQ_NORETCALL(
#                     functionName=MKAbsyn.CREF_IDENT(name="reinit"),
#                     functionArgs=MKAbsyn.FUNCTIONARGS(args=e1 <| e2 <| nil(), argNames=nil()),
#                 ) => begin
#                     SCode.EQ_REINIT(e1, e2, inComment, inInfo)
#                 end

#                 MKAbsyn.EQ_NORETCALL(__) => begin
#                     SCode.EQ_NORETCALL(
#                         MKAbsyn.CALL(inEquation.functionName, inEquation.functionArgs, nil),
#                         inComment,
#                         inInfo,
#                     )
#                 end
#             end
#         end
#         return outEEquation
#     end

#     function translateEqBranch(
#         inBranch::Tuple{<:MKAbsyn.Exp,List{<:MKAbsyn.EquationItem}},
#         inIsInitial::Bool,
#     )::Tuple{MKAbsyn.Exp,List{SCode.EEquation}}
#         local outBody::List{SCode.EEquation}
#         local outCondition::MKAbsyn.Exp

#         local body::List{MKAbsyn.EquationItem}

#         (outCondition, body) = inBranch
#         outBody = translateEEquations(body, inIsInitial)
#         return (outCondition, outBody)
#     end

#     function translateIterator(
#         inIterator::MKAbsyn.ForIterator,
#         inInfo::SourceInfo,
#     )::Tuple{String,Option{MKAbsyn.Exp}}
#         local outRange::Option{MKAbsyn.Exp}
#         local outName::String

#         local guard_exp::Option{MKAbsyn.Exp}

#         @match MKAbsyn.ITERATOR(name=outName, guardExp=guard_exp, range=outRange) = inIterator
#         if isSome(guard_exp)
#         end
#         return (outName, outRange)
#     end

#     """ #= function: translateElementAddinfo =#"""
#     function translateElementAddinfo(elem::SCode.Element, nfo::SourceInfo)::SCode.Element
#         local oelem::SCode.Element

#         oelem = begin
#             local a1::SCode.Ident
#             local a2::MKAbsyn.InnerOuter
#             local a3::Bool
#             local a4::Bool
#             local a5::Bool
#             local rd::Bool
#             local a6::SCode.Attributes
#             local a7::MKAbsyn.TypeSpec
#             local a8::SCode.Mod
#             local a10::SCode.Comment
#             local a11::Option{MKAbsyn.Exp}
#             local a13::Option{MKAbsyn.ConstrainClass}
#             local p::SCode.Prefixes
#             @match (elem, nfo) begin
#                 (SCode.COMPONENT(a1, p, a6, a7, a8, a10, a11, _), _) => begin
#                     SCode.COMPONENT(a1, p, a6, a7, a8, a10, a11, nfo)
#                 end

#                 _ => begin
#                     elem
#                 end
#             end
#         end
#         return oelem
#     end

#     #= /* Modification management */ =#

#     """ #= Builds an SCode.Mod from an MKAbsyn.Modification. =#"""
#     function translateMod(
#         inMod::Option{<:MKAbsyn.Modification},
#         finalPrefix::SCode.Final,
#         eachPrefix::SCode.Each,
#         info::SourceInfo,
#     )::SCode.Mod
#         local outMod::SCode.Mod

#         local args::List{MKAbsyn.ElementArg}
#         local eqmod::MKAbsyn.EqMod
#         local subs::List{SCode.SubMod}
#         local binding::Option{MKAbsyn.Exp}

#         (args, eqmod) = begin
#             @match inMod begin
#                 SOME(MKAbsyn.CLASSMOD(elementArgLst=args, eqMod=eqmod)) => begin
#                     (args, eqmod)
#                 end

#                 _ => begin
#                     (nil, MKAbsyn.NOMOD())
#                 end
#             end
#         end
#         subs = if listEmpty(args)
#             nil
#         else
#             translateArgs(args)
#         end
#         binding = begin
#             @match eqmod begin
#                 MKAbsyn.EQMOD(__) => begin
#                     SOME(eqmod.exp)
#                 end

#                 _ => begin
#                     NONE()
#                 end
#             end
#         end
#         outMod = begin
#             @match (subs, binding, finalPrefix, eachPrefix) begin
#                 (nil(), NONE(), SCode.NOT_FINAL(__), SCode.NOT_EACH(__)) => begin
#                     SCode.NOMOD()
#                 end

#                 _ => begin
#                     SCode.MOD(finalPrefix, eachPrefix, subs, binding, info)
#                 end
#             end
#         end
#         return outMod
#     end

#     function translateArgs(args::List{<:MKAbsyn.ElementArg})::List{SCode.SubMod}
#         local subMods::List{SCode.SubMod} = nil

#         local smod::SCode.Mod
#         local elem::SCode.Element
#         local sub::SCode.SubMod

#         for arg in args
#             subMods = begin
#                 @match arg begin
#                     MKAbsyn.MODIFICATION(__) => begin
#                         smod = translateMod(
#                             arg.modification,
#                             SCodeUtil.boolFinal(arg.finalPrefix),
#                             translateEach(arg.eachPrefix),
#                             arg.info,
#                         )
#                         if !SCodeUtil.isEmptyMod(smod)
#                             sub = translateSub(arg.path, smod, arg.info)
#                             subMods = _cons(sub, subMods)
#                         end
#                         subMods
#                     end

#                     MKAbsyn.REDECLARATION(__) => begin
#                         @match _cons(elem, nil) = translateElementspec(
#                             arg.constrainClass,
#                             arg.finalPrefix,
#                             MKAbsyn.NOT_INNER_OUTER(),
#                             SOME(arg.redeclareKeywords),
#                             SCode.PUBLIC(),
#                             arg.elementSpec,
#                             arg.info,
#                         )
#                         sub = SCode.NAMEMOD(
#                             MKAbsynUtil.elementSpecName(arg.elementSpec),
#                             SCode.REDECL(
#                                 SCodeUtil.boolFinal(arg.finalPrefix),
#                                 translateEach(arg.eachPrefix),
#                                 elem,
#                             ),
#                         )
#                         _cons(sub, subMods)
#                     end
#                 end
#             end
#         end
#         subMods = listReverse(subMods)
#         return subMods
#     end

#     """ #= This function converts a MKAbsyn.ComponentRef plus a list
#     of modifications into a number of nested SCode.SUBMOD. =#"""
#     function translateSub(inPath::MKAbsyn.Path, inMod::SCode.Mod, info::SourceInfo)::SCode.SubMod
#         local outSubMod::SCode.SubMod

#         outSubMod = begin
#             local i::String
#             local path::MKAbsyn.Path
#             local mod::SCode.Mod
#             local sub::SCode.SubMod
#             #=  Then the normal rules
#             =#
#             @match (inPath, inMod, info) begin
#                 (MKAbsyn.IDENT(name=i), mod, _) => begin
#                     SCode.NAMEMOD(i, mod)
#                 end

#                 (MKAbsyn.QUALIFIED(name=i, path=path), mod, _) => begin
#                     sub = translateSub(path, mod, info)
#                     mod =
#                         SCode.MOD(SCode.NOT_FINAL(), SCode.NOT_EACH(), list(sub), NONE(), info)
#                     SCode.NAMEMOD(i, mod)
#                 end
#             end
#         end
#         return outSubMod
#     end

#     function makeTypeVarElement(str::String, info::SourceInfo)::SCode.Element
#         local elt::SCode.Element

#         local cd::SCode.ClassDef
#         local ts::MKAbsyn.TypeSpec

#         ts = MKAbsyn.TCOMPLEX(
#             MKAbsyn.IDENT("polymorphic"),
#             list(MKAbsyn.TPATH(MKAbsyn.IDENT("Any"), NONE())),
#             NONE(),
#         )
#         cd = SCode.DERIVED(
#             ts,
#             SCode.NOMOD(),
#             SCode.ATTR(
#                 nil,
#                 SCode.POTENTIAL(),
#                 SCode.NON_PARALLEL(),
#                 SCode.VAR(),
#                 MKAbsyn.BIDIR(),
#                 MKAbsyn.NONFIELD(),
#                 false
#             ),
#         )
#         elt = SCode.CLASS(
#             str,
#             SCode.PREFIXES(
#                 SCode.PUBLIC(),
#                 SCode.NOT_REDECLARE(),
#                 SCode.FINAL(),
#                 MKAbsyn.NOT_INNER_OUTER(),
#                 SCode.NOT_REPLACEABLE(),
#             ),
#             SCode.NOT_ENCAPSULATED(),
#             SCode.NOT_PARTIAL(),
#             SCode.R_TYPE(),
#             cd,
#             SCode.noComment,
#             info,
#         )
#         return elt
#     end

#     function translateEach(inAEach::MKAbsyn.Each)::SCode.Each
#         local outSEach::SCode.Each

#         outSEach = begin
#             @match inAEach begin
#                 MKAbsyn.EACH(__) => begin
#                     SCode.EACH()
#                 end

#                 MKAbsyn.NON_EACH(__) => begin
#                     SCode.NOT_EACH()
#                 end
#             end
#         end
#         return outSEach
#     end

#     function checkTypeSpec(ts::MKAbsyn.TypeSpec, info::SourceInfo)
#         _ = begin
#             local tss::List{MKAbsyn.TypeSpec}
#             local ts2::MKAbsyn.TypeSpec
#             local str::String
#             @match (ts, info) begin
#                 (MKAbsyn.TPATH(__), _) => begin
#                     ()
#                 end

#                 (MKAbsyn.TCOMPLEX(path=MKAbsyn.IDENT("tuple"), typeSpecs=ts2 <| nil()), _) => begin
#                     str = MKAbsynUtil.typeSpecString(ts)
#                     checkTypeSpec(ts2, info)
#                     ()
#                 end

#                 (MKAbsyn.TCOMPLEX(path=MKAbsyn.IDENT("tuple"), typeSpecs=tss && _ <| _ <| _), _) =>
#                     begin
#                         ListUtil.map1_0(tss, checkTypeSpec, info)
#                         ()
#                     end

#                 (MKAbsyn.TCOMPLEX(typeSpecs=ts2 <| nil()), _) => begin
#                     checkTypeSpec(ts2, info)
#                     ()
#                 end

#                 (MKAbsyn.TCOMPLEX(typeSpecs=tss), _) => begin
#                     if listMember(
#                         ts.path,
#                         list(
#                             MKAbsyn.IDENT("list"),
#                             MKAbsyn.IDENT("List"),
#                             MKAbsyn.IDENT("array"),
#                             MKAbsyn.IDENT("Array"),
#                             MKAbsyn.IDENT("polymorphic"),
#                             MKAbsyn.IDENT("Option"),
#                         ),
#                     )
#                         str = MKAbsynUtil.typeSpecString(ts)
#                         ListUtil.map1_0(tss, checkTypeSpec, info)
#                     end
#                     ()
#                 end
#             end
#         end

#     end

@exportAll
end
