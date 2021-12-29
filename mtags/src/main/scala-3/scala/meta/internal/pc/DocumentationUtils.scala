package scala.meta.internal.pc

import org.eclipse.lsp4j.MarkupContent
import dotty.tools.dotc.util.ParsedComment
import dotty.tools.dotc.core.Contexts.Context

/**
 * Utilities for formatting documentation into lsp4j.MarkupContent
 *
 *  @see org.eclipse.lsp4j.MarkupContent
 */
object DocumentationUtils:
  def hoverContent(
      typeInfo: Option[String],
      comments: List[ParsedComment]
  )(using ctx: Context): MarkupContent =
    val buf = new StringBuilder
    typeInfo.foreach { info =>
      buf.append(s"""```scala
                    |$info
                    |```
                    |""".stripMargin)
    }
    comments.foreach { comment => buf.append(comment.renderAsMarkdown) }
    markupContent(buf.toString)
  end hoverContent

  def markupContent(content: String): MarkupContent =
    if content.isEmpty then null
    else
      val markup = new MarkupContent
      markup.setKind("markdown")
      markup.setValue(content.trim)
      markup
end DocumentationUtils
