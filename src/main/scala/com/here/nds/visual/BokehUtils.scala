package com.here.nds.visual


import io.continuum.bokeh.{Line => BokehLine, _}

import scala.collection.immutable.IndexedSeq


object BokehUtils {

  /**
   *
   * @param xdr
   * @param ydr
   * @param tools all Tools
   *              val panTool = new PanTool().plot(plot)
   *              val wheelZoomTool = new WheelZoomTool().plot(plot)
   *              val previewSaveTool = new PreviewSaveTool().plot(plot)
   *              val resetTool = new ResetTool().plot(plot)
   *              val resizeTool = new ResizeTool().plot(plot)
   *              val crosshairTool = new CrosshairTool().plot(plot)
   *              plot.tools := List(panTool, wheelZoomTool, previewSaveTool, resetTool, resizeTool, crosshairTool)
   * @param width
   * @param height
   */
  def getPlot(xdr: DataRange, ydr: DataRange, tools: List[Tool], width: Int = 800, height: Int = 400) = {
    new Plot().x_range(xdr).y_range(ydr).tools(tools).width(width).height(height)
  }

  def getLinearAxis(plot: Plot, position: Location): ContinuousAxis = {
    getAxis(plot, new LinearAxis, position)
  }

  /**
   * get datetime axis
   *
   * @param plot
   * @param position
   * @param formatter eg. new DatetimeTickFormatter().formats(Map(DatetimeUnits.Months -> List("%b %Y")))
   * @return
   */
  def getDatetimeAxis(plot: Plot, position: Location, formatter: DatetimeTickFormatter = new DatetimeTickFormatter().formats(Map(DatetimeUnits.Months -> List("%b %Y")))): ContinuousAxis = {
    getAxis(plot, new DatetimeAxis().formatter(formatter), position)
  }

  def getAxis(plot: Plot, axisType: ContinuousAxis, position: Location): ContinuousAxis = {
    val axis = axisType.plot(plot).location(position)
    setPlotAxis(plot, axis, position)
    setRenderer(plot, axis)
    axis
  }

  def setAxisLabel(axis: ContinuousAxis, axisLabel: String) = {
    axis.axis_label(axisLabel)
  }

  def setPlotAxis(plot: Plot, axis: ContinuousAxis, position: Location) {
    position match {
      case Location.Left => plot.left <<= (axis :: _)
      case Location.Above => plot.above <<= (axis :: _)
      case Location.Below => plot.below <<= (axis :: _)
      case Location.Right => plot.right <<= (axis :: _)
      case _ =>
    }
  }

  def getCircleGlyph(column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val circle = new Circle().x(column_x).y(column_y).size(size).fill_color(fill_Color).line_color(line_Color)
    getGlyphRenderer(value, circle)
  }
  def setCircleGlyph(plot: Plot, column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val circleGlyph = getCircleGlyph(column_x, column_y, value, size, fill_Color, line_Color)
    setRenderer(plot, circleGlyph).asInstanceOf[GlyphRenderer]
  }

  def getLineGlyph(column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, width: Int = 3, line_Color: Color = Color.Black) = {
    val line = new BokehLine().x(column_x).y(column_y).line_width(width).line_color(line_Color)
    getGlyphRenderer(value, line)
  }

  def setLineGlyph(plot: Plot, column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, width: Int = 3, line_Color: Color = Color.Black) = {
    val lineGlyph = getLineGlyph(column_x, column_y, value, width, line_Color)
    setRenderer(plot, lineGlyph).asInstanceOf[GlyphRenderer]
  }

  def getPatchGlyph(column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, width: Int = 3, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val patch = new Patch().x(column_x).y(column_y).line_width(width).line_color(line_Color).fill_color(fill_Color)
    getGlyphRenderer(value, patch)
  }

  def setPatchGlyph(plot: Plot, column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, width: Int = 3, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val patchGlyph = getPatchGlyph(column_x, column_y, value, width, fill_Color, line_Color)
    setRenderer(plot, patchGlyph).asInstanceOf[GlyphRenderer]
  }

  def getCircleCrossGlyph(column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val circleCross = new CircleCross().x(column_x).y(column_y).size(size).fill_color(fill_Color).line_color(line_Color)
    getGlyphRenderer(value, circleCross)
  }

  def setCircleCrossGlyph(plot: Plot, column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val circleCrossGlyph = getCircleCrossGlyph(column_x, column_y, value, size, fill_Color, line_Color)
    setRenderer(plot, circleCrossGlyph).asInstanceOf[GlyphRenderer]
  }

  def getTextGlyph(column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val text = new Text().x(column_x).y(column_y).text("1")
    getGlyphRenderer(value, text)
  }

  def setTextGlyph(plot: Plot, column_x: ColumnDataSource#Column[IndexedSeq, Double], column_y: ColumnDataSource#Column[IndexedSeq, Double], value: DataSource, size: Int = 5, fill_Color: Color = Color.Red, line_Color: Color = Color.Black) = {
    val textGlyph = getTextGlyph(column_x, column_y, value, size, fill_Color, line_Color)
    setRenderer(plot, textGlyph).asInstanceOf[GlyphRenderer]
  }

  def getGlyphRenderer(value: DataSource, glyph: Glyph) = {
    new GlyphRenderer().data_source(value).glyph(glyph)
  }

  /**
   *
   * @param legends eg.  val legends = List("y = sin(x)" -> List(lineGlyph, circleGlyph))
   */
  def getLegends(plot: Plot, legends: List[(String, List[GlyphRenderer])]): Legend = {
    val legend = new Legend().plot(plot).legends(legends)
    setRenderer(plot, legend)
    legend
  }

  def getLegends(plot: Plot, name: String, glyphList: List[GlyphRenderer]): Legend = {
    getLegends(plot, List(name -> glyphList))
  }

  /**
   *
   * @param plot
   * @param axis
   * @param dimension 0 means x and 1 means y
   * @return
   */
  def getGrid(plot: Plot, axis: ContinuousAxis, dimension: Int) = {
    val grid = new Grid().plot(plot).dimension(dimension).axis(axis)
    setRenderer(plot, grid)
    grid
  }

  def setRenderers(plot: Plot, renderers: List[Renderer] => List[Renderer]) = {
    plot.renderers <<= renderers
  }

  def setRenderer(plot: Plot, renderer: Renderer) = {
    val renderers: (List[Renderer] => List[Renderer]) = (renderer :: _)
    setRenderers(plot, renderers)
    renderer
  }

  /**
   * use this method just can plot one renderer
   *
   * @param plot
   * @param renderers
   */
  def setRenderers(plot: Plot, renderers: List[Renderer]) = {
    plot.renderers := renderers
  }

  /**
   * use gridplot Multiple plots in the document
   *
   * @param children every child List is one row   eg. val children = List(List(microsoftPlot, bofaPlot), List(caterPillarPlot, mmmPlot))
   * @return
   */
  def multiplePlots(children: List[List[Plot]], title: String = ""): Plot = {
    new GridPlot().children(children).title(title)
  }

  def save2Document(plot: Plot, path: String = "sample.html"): Unit = {
    val document = new Document(plot)
    val html = document.save(path)
    println(s"Wrote ${html.file}. Open ${html.url} in a web browser.")
    html.view()
  }
}
