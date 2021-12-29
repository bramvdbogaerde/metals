package scala.meta.internal.pc

import scala.meta.pc.OffsetParams
import scala.meta.internal.pc.AutoImports.*
import scala.meta.pc.PresentationCompilerConfig
import org.eclipse.lsp4j.TextEdit
import org.eclipse.{lsp4j as l}
import dotty.tools.dotc.interactive.*
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.ast.tpd.Template
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.core.Denotations.Denotation
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Scopes.Scope
import scala.meta.internal.mtags.MtagsEnrichments.*

import dotty.tools.dotc.core.Types.TermRef
import scala.collection.mutable
import dotty.tools.dotc.core.Symbols.Symbol

// TODO: when generating the edit text, we might need to prepend an "override" modifier if the method being overriden is already concrete (or possibly concrete) in a superclass
// TODO: are overloaded symbols properly autocompleted?
// TODO: the user cannot override val with a var, so only vars should override vars.
class OverrideCompletion(
    name: TermName,
    t: Template,
    pos: SourcePosition,
    path: List[Tree]
)(using ctx: Context, pcCtx: PresentationCompilerContext):

  def contribute(): List[CompletionValue] =
    t.tpe match
      case tr: TermRef =>
        val prefix = tr.prefix
        val localDecls = prefix.decls
        val members =
          prefix
            .memberNames(Types.takeAllFilter)
            .map(tr.prefix.member(_))
            .filter(interestingMember(name, localDecls))

        members.map { member =>
          // TODO: determine label based on what kind we are overriding. We do not want to show override def if
          // we are actually overriding a val
          OverrideCompletionValue(
            s"override def ${member.symbol.showName}", // name is sufficient as a label
            member.symbol
          )
        }.toList

      case _ =>
        pcCtx.logger.log("not matched")
        List()

  /** Returns whether the method is a "var" definition */
  private def isVarSetter(sym: Symbol): Boolean =
    sym.is(StableRealizable) && !sym.is(Lazy) && sym.is(Accessor)

  /**
   *  Returns true if the member is "interesting".
   *
   *  @param name the name that we are overriding
   *  @param member the member for which we need to decide whether it is interesting
   *  @return true if the member is a method or field that can be overriden from a parent
   */
  private def interestingMember(name: TermName, localDecls: Scope)(
      member: Denotation
  ): Boolean =
    // TODO: do not suggest the methods in the local class
    val sym = member.symbol
    sym.is(Method) &&
    !sym.is(Private) &&
    !isVarSetter(sym) || (isVarSetter(sym) && sym.is(Deferred)) &&
    !sym.is(Final) &&
    !sym.is(Synthetic) &&
    !sym.is(Artifact) &&
    sym.showName.startsWith(name.toString)
  end interestingMember

  case class OverrideCompletionValue(label: String, override val symbol: Symbol)
      extends CompletionValue:

    override def kind = CompletionValue.Kind.Override

    def update(
        symbol: Symbol = this.symbol,
        ignored: CompletionValue.Kind = this.kind
    ): CompletionValue = this.copy(symbol = symbol)

    def asItem(
        range: l.Range,
        idx: Int,
        path: List[Tree],
        autoImports: AutoImportsGenerator,
        indexedContext: IndexedContext,
        history: ShortenedNames
    )(using
        ctx: Context
    ): l.CompletionItem =
      val item = prepareItem(label, symbol, idx, history)
      val edit = TextEdit(range, buildCompletionText(history))
      item.setTextEdit(edit)
      item
    end asItem

    /* Build the actual text that will be used as an edit */
    def buildCompletionText(history: ShortenedNames)(using
        ctx: Context
    ): String =
      val printer = SymbolPrinter()
      val info = symbol.info.widenTermRefExpr
      val typeSymbol = info.typeSymbol
      val signature =
        printer.defaultMethodSignature(symbol, history, info, true)
      val name = symbol.showName
      s"${symbol.showName}$signature = ???"

  end OverrideCompletionValue

end OverrideCompletion
