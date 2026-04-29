                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package larceny

object CompileError:
  def apply(ordinal: Int, message: String, code: String, start: Int, offset: Int): CompileError =
    new CompileError(CompileError.Reason.fromOrdinal(ordinal), message, code, start, offset)

  enum Reason:
    case NoExplanation
    case EmptyCatchOrFinallyBlock                                                          // old
    case EmptyCatchBlock
    case EmptyCatchAndFinallyBlock
    case DeprecatedWithOperator
    case CaseClassMissingParamList
    case DuplicateBind
    case MissingIdent
    case TypeMismatch
    case NotAMember
    case EarlyDefinitionsNotSupported
    case TopLevelImplicitClass                                                             // old
    case ImplicitCaseClass
    case ImplicitClassPrimaryConstructorArity
    case ObjectMayNotHaveSelfType
    case TupleTooLong                                                                      // old
    case RepeatedModifier
    case InterpolatedStringError
    case UnboundPlaceholderParameter
    case IllegalStartSimpleExpr
    case MissingReturnType
    case YieldOrDoExpectedInForComprehension
    case ProperDefinitionNotFound
    case ByNameParameterNotSupported
    case WrongNumberOfTypeArgs
    case IllegalVariableInPatternAlternative
    case IdentifierExpected
    case AuxConstructorNeedsNonImplicitParameter
    case VarArgsParamMustComeLast
    case IllegalLiteral
    case PatternMatchExhaustivity
    case MatchCaseUnreachable
    case SeqWildcardPatternPos
    case IllegalStartOfSimplePattern
    case PkgDuplicateSymbol
    case ExistentialTypesNoLongerSupported
    case UnboundWildcardType
    case DanglingThisInPath                                                                // old
    case OverridesNothing
    case OverridesNothingButNameExists
    case ForwardReferenceExtendsOverDefinition
    case ExpectedTokenButFound
    case MixedLeftAndRightAssociativeOps
    case CantInstantiateAbstractClassOrTrait
    case UnreducibleApplication
    case OverloadedOrRecursiveMethodNeedsResultType
    case RecursiveValueNeedsResultType
    case CyclicReferenceInvolving
    case CyclicReferenceInvolvingImplicit
    case SuperQualMustBeParent
    case AmbiguousReference
    case MethodDoesNotTakeParameters
    case AmbiguousOverload
    case ReassignmentToVal
    case TypeDoesNotTakeParameters
    case ParameterizedTypeLacksArguments                                                   // old
    case VarValParametersMayNotBeCallByName
    case MissingTypeParameterFor
    case DoesNotConformToBound
    case DoesNotConformToSelfType
    case DoesNotConformToSelfTypeCantBeInstantiated
    case AbstractMemberMayNotHaveModifier
    case TopLevelCantBeImplicit                                                            // old
    case TypesAndTraitsCantBeImplicit
    case OnlyClassesCanBeAbstract
    case AbstractOverrideOnlyInTraits
    case TraitsMayNotBeFinal
    case NativeMembersMayNotHaveImplementation
    case OnlyClassesCanHaveDeclaredButUndefinedMembers
    case CannotExtendAnyVal
    case CannotHaveSameNameAs
    case ValueClassesMayNotDefineInner
    case ValueClassesMayNotDefineNonParameterField
    case ValueClassesMayNotDefineASecondaryConstructor
    case ValueClassesMayNotContainInitalization
    case ValueClassesMayNotBeAbstract
    case ValueClassesMayNotBeContainted
    case ValueClassesMayNotWrapAnotherValueClass
    case ValueClassParameterMayNotBeAVar
    case ValueClassNeedsExactlyOneValParam
    case OnlyCaseClassOrCaseObjectAllowed                                                  // old
    case ExpectedTopLevelDef                                                               // old
    case AnonymousFunctionMissingParamType
    case SuperCallsNotAllowedInlineable
    case NotAPath
    case WildcardOnTypeArgumentNotAllowedOnNew
    case FunctionTypeNeedsNonEmptyParameterList
    case WrongNumberOfParameters
    case DuplicatePrivateProtectedQualifier
    case ExpectedStartOfTopLevelDefinition
    case MissingReturnTypeWithReturnStatement
    case NoReturnFromInlineable
    case ReturnOutsideMethodDefinition
    case UncheckedTypePattern
    case ExtendFinalClass
    case EnumCaseDefinitionInNonEnumOwner
    case ExpectedTypeBoundOrEquals
    case ClassAndCompanionNameClash
    case TailrecNotApplicable
    case FailureToEliminateExistential
    case OnlyFunctionsCanBeFollowedByUnderscore
    case MissingEmptyArgumentList
    case DuplicateNamedTypeParameter
    case UndefinedNamedTypeParameter
    case IllegalStartOfStatement
    case TraitIsExpected
    case TraitRedefinedFinalMethodFromAnyRef
    case PackageNameAlreadyDefined
    case UnapplyInvalidNumberOfArguments
    case UnapplyInvalidReturnType
    case StaticFieldsOnlyAllowedInObjects
    case CyclicInheritance
    case BadSymbolicReference
    case UnableToExtendSealedClass
    case SymbolHasUnparsableVersionNumber
    case SymbolChangedSemanticsInVersion
    case UnableToEmitSwitch
    case MissingCompanionForStatic
    case PolymorphicMethodMissingTypeInParent
    case ParamsNoInline
    case JavaSymbolIsNotAValue
    case DoubleDefinition
    case MatchCaseOnlyNullWarning
    case ImportedTwice
    case TypeTestAlwaysDiverges
    case TermMemberNeedsNeedsResultTypeForImplicitSearch
    case ClassCannotExtendEnum
    case ValueClassParameterMayNotBeCallByName
    case NotAnExtractor
    case MemberWithSameNameAsStatic
    case PureExpressionInStatementPosition
    case TraitCompanionWithMutableStatic
    case LazyStaticField
    case StaticOverridingNonStaticMembers
    case OverloadInRefinement
    case NoMatchingOverload
    case StableIdentPattern
    case StaticFieldsShouldPrecedeNonStatic
    case IllegalSuperAccessor
    case TraitParameterUsedAsParentPrefix
    case UnknownNamedEnclosingClassOrObject
    case IllegalCyclicTypeReference
    case MissingTypeParameterInTypeApp
    case SkolemInInferred
    case ErasedTypesCanOnlyBeFunctionTypes
    case CaseClassMissingNonImplicitParamList
    case EnumerationsShouldNotBeEmpty
    case IllegalParameterInit
    case RedundantModifier
    case TypedCaseDoesNotExplicitlyExtendTypedEnum
    case IllegalRedefinitionOfStandardKind
    case NoExtensionMethodAllowed
    case ExtensionMethodCannotHaveTypeParams
    case ExtensionCanOnlyHaveDefs
    case UnexpectedPatternForSummonFrom
    case AnonymousInstanceCannotBeEmpty
    case TypeSpliceInValPattern                                                            // old
    case ModifierNotAllowedForDefinition
    case CannotExtendJavaEnum
    case InvalidReferenceInImplicitNotFoundAnnotation
    case TraitMayNotDefineNativeMethod
    case JavaEnumParentArgs
    case AlreadyDefined
    case CaseClassInInlinedCode
    case OverrideTypeMismatchError                                                         // old
    case OverrideError
    case MatchableWarning
    case CannotExtendFunction
    case LossyWideningConstantConversion
    case ImplicitSearchTooLarge
    case TargetNameOnTopLevelClass
    case NotClassType
    case MissingArgument
    case MissingImplicitArgument
    case CannotBeAccessed
    case InlineGivenShouldNotBeFunction
    case ValueDiscarding
    case UnusedNonUnitValue
    case ConstrProxyShadows
    case MissingArgumentList
    case MatchTypeScrutineeCannotBeHigherKinded
    case AmbiguousExtensionMethod
    case UnqualifiedCallToAnyRefMethod
    case NotConstant
    case ClosureCannotHaveInternalParameterDependencies
    case MatchTypeNoCases
    case UnimportedAndImported
    case ImplausiblePatternWarning
    case SynchronizedCallOnBoxedClass
    case VarArgsParamCannotBeGiven
    case ExtractorNotFound
    case PureUnitExpression
    case MatchTypeLegacyPattern
    case UnstableInlineAccessor
    case VolatileOnVal
    case ExtensionNullifiedByMember
    case ConstructorProxyNotValue
    case ContextBoundCompanionNotValue
    case InlinedAnonClassWarning
    case UnusedSymbol
    case TailrecNestedCal
    case FinalLocalDef
    case NonNamedArgumentInJavaAnnotation
    case QuotedTypeMissing
    case DeprecatedAssignmentSyntax
    case DeprecatedInfixNamedArgumentSyntax
    case GivenSearchPriority
    case EnumMayNotBeValueClasses
    case IllegalUnrollPlacement
    case ExtensionHasDefault
    case FormatInterpolationError
    case ValueClassCannotExtendAliasOfAnyVal
    case MatchIsNotPartialFunction

case class CompileError
  ( reason: CompileError.Reason, message: String, code: String, start: Int, offset: Int ):

  def point: Int = start + offset
