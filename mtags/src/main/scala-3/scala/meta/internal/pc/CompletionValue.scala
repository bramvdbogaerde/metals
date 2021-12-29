package scala.meta.internal.pc

import scala.meta.internal.pc.CompletionValue.Kind

import scala.collection.JavaConverters.*
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.interactive.Completion
import scala.meta.internal.mtags.MtagsEnrichments.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.util.ParsedComment
import scala.meta.internal.pc.AutoImports.*
import dotty.tools.dotc.transform.SymUtils.*

import org.eclipse.lsp4j as l
import org.eclipse.lsp4j.{
  CompletionItemKind,
  TextEdit,
  CompletionItem,
  CompletionItemTag
}

/**
 *  A completion value is a value that is able to be converted to a completion item
 */
trait CompletionValue:
  /** The symbol that this completion is completing */
  def symbol: Symbol

  /** The kind of completion */
  def kind: CompletionValue.Kind

  /**
   * Update can be used to update the associated symbol and kind of this completion value.
   *
   *  Note that some completion values might not allow changing the kind
   */
  def update(
      symbol: Symbol = this.symbol,
      kind: CompletionValue.Kind = this.kind
  ): CompletionValue

  /**
   *  Convert the completion value to an item
   */
  def asItem(
      range: l.Range,
      idx: Int,
      path: List[Tree],
      autoImports: AutoImportsGenerator,
      indexedContext: IndexedContext,
      history: ShortenedNames
  )(using
      ctx: Context
  ): l.CompletionItem

  /**
   *  Prepares an item, it sets up the kind of the item as well as the documentation of the item,
   *  but leaves the TextEdit associated with the completion untouched
   */
  protected def prepareItem(
      ident: String,
      sym: Symbol,
      idx: Int,
      history: ShortenedNames,
      isFromWorkspace: Boolean = false,
      additionalEdits: List[TextEdit] = Nil
  )(using ctx: Context): CompletionItem =
    import DocumentationUtils.*

    // For overloaded signatures we get multiple symbols, so we need
    // to recalculate the description
    // related issue https://github.com/lampepfl/dotty/issues/11941
    lazy val kind: l.CompletionItemKind = completionItemKind(sym)

    val printer = SymbolPrinter()(using ctx)
    val description = printer.completionDetailString(sym, history)

    val label =
      kind match
        case CompletionItemKind.Method =>
          s"${ident}${description}"
        case CompletionItemKind.Variable | CompletionItemKind.Field =>
          s"${ident}: ${description}"
        case CompletionItemKind.Module | CompletionItemKind.Class =>
          if isFromWorkspace then s"${ident} -${description}"
          else s"${ident}${description}"
        case _ =>
          ident
    val item = new CompletionItem(label)

    item.setSortText(f"${idx}%05d")
    item.setDetail(description)
    item.setFilterText(label)

    item.setAdditionalTextEdits(additionalEdits.asJava)

    val documentation = ParsedComment.docOf(sym)
    if documentation.nonEmpty then
      item.setDocumentation(hoverContent(None, documentation.toList))

    if sym.isDeprecated then
      item.setTags(List(CompletionItemTag.Deprecated).asJava)

    item.setKind(completionItemKind(sym))
    item
  end prepareItem

  protected def completionItemKind(
      sym: Symbol
  )(using ctx: Context): l.CompletionItemKind =
    if sym.is(Package) || sym.is(Module) then
      l.CompletionItemKind.Module // No CompletionItemKind.Package (https://github.com/Microsoft/language-server-protocol/issues/155)
    else if sym.isConstructor then l.CompletionItemKind.Constructor
    else if sym.isClass then l.CompletionItemKind.Class
    else if sym.is(Mutable) then l.CompletionItemKind.Variable
    else if sym.is(Method) then l.CompletionItemKind.Method
    else l.CompletionItemKind.Field
  end completionItemKind

end CompletionValue

case class SimpleCompletionValue(
    label: String,
    override val symbol: Symbol,
    kind: Kind
) extends CompletionValue:
  import DocumentationUtils.*

  def update(
      symbol: Symbol = this.symbol,
      kind: CompletionValue.Kind = this.kind
  ): CompletionValue =
    this.copy(symbol = symbol, kind = kind)

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
    val sym = symbol

    def mkItem0(
        ident: String,
        sym: Symbol,
        nameEdit: TextEdit,
        idx: Int,
        isFromWorkspace: Boolean = false,
        additionalEdits: List[TextEdit] = Nil
    )(using Context): CompletionItem =
      val item = prepareItem(
        ident,
        sym,
        idx,
        history,
        isFromWorkspace,
        additionalEdits
      )
      item.setTextEdit(nameEdit)
      item
    end mkItem0

    def mkItem(
        ident: String,
        value: String,
        isFromWorkspace: Boolean = false,
        additionalEdits: List[TextEdit] = Nil
    ): CompletionItem =
      val nameEdit = new TextEdit(
        range,
        value
      )
      mkItem0(
        ident,
        symbol,
        nameEdit,
        idx,
        isFromWorkspace,
        additionalEdits
      )

    end mkItem

    def mkWorkspaceItem(
        ident: String,
        value: String,
        additionalEdits: List[TextEdit] = Nil
    ): CompletionItem =
      mkItem(ident, value, isFromWorkspace = true, additionalEdits)

    val ident = label
    this.kind match
      case CompletionValue.Kind.Workspace =>
        path match
          case (_: Ident) :: (_: Import) :: _ =>
            mkWorkspaceItem(
              ident,
              sym.fullNameBackticked
            )
          case _ =>
            autoImports.editsForSymbol(sym) match
              case Some(edits) =>
                edits match
                  case AutoImportEdits(Some(nameEdit), other) =>
                    mkItem0(
                      ident,
                      this.symbol,
                      nameEdit,
                      idx,
                      isFromWorkspace = true,
                      other.toList
                    )
                  case _ =>
                    mkItem(
                      ident,
                      ident.backticked,
                      isFromWorkspace = true,
                      edits.edits
                    )
              case None =>
                val r = indexedContext.lookupSym(sym)
                r match
                  case IndexedContext.Result.InScope =>
                    mkItem(ident, ident.backticked)
                  case _ => mkWorkspaceItem(ident, sym.fullNameBackticked)
      case CompletionValue.Kind.NamedArg => mkItem(ident, ident)
      case _ => mkItem(ident, ident.backticked)
    end match
  end asItem

end SimpleCompletionValue

object CompletionValue:

  enum Kind:
    case Override, NamedArg, Workspace, Compiler, Scope

  def fromCompiler(completion: Completion): List[CompletionValue] =
    completion.symbols.map(
      SimpleCompletionValue(completion.label, _, Kind.Compiler)
    )

  def namedArg(label: String, sym: Symbol): CompletionValue =
    SimpleCompletionValue(label, sym, Kind.NamedArg)

  def workspace(label: String, sym: Symbol): CompletionValue =
    SimpleCompletionValue(label, sym, Kind.Workspace)

  def scope(label: String, sym: Symbol): CompletionValue =
    SimpleCompletionValue(label, sym, Kind.Scope)

  def overrideValue(label: String, sym: Symbol): CompletionValue =
    SimpleCompletionValue(label, sym, Kind.Override)

end CompletionValue
