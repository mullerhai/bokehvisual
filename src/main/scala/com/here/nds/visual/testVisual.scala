package com.here.nds.visual



import io.continuum.bokeh.Tools._
import io.continuum.bokeh._

import scala.collection.immutable.IndexedSeq
import scala.math.{sin, Pi => pi}


/**
 * Created by muller
 * http://bokeh.pydata.org/en/latest/docs/user_guide/quickstart.html
 */
object testVisual extends App {

  val xdr = new DataRange1d()
  val ydr = new DataRange1d()

  object source extends ColumnDataSource {
    val seq = -2 * pi to 2 * pi by 0.1
    seq.toIndexedSeq
    val x: ColumnDataSource#Column[IndexedSeq, Double] = column(seq.toIndexedSeq)
    val y = column(x.value.map(sin))
    val z = column(x.value.map(Math.pow(2, _)))
    val p = column(x.value.map(Math.pow(3, _)))
    //    val x = column(-10.0 to 10 by 0.1)
    //    val y = column(-10.0 to 5 by 0.1)
  }

  import source.{p, x, y, z}

  //  val plot = plotOne("全图")
  //  BokehHelper.save2Document(plot = plot)

  val plot = plotMulitple()
  BokehUtils.save2Document(plot)

  def plotMulitple() = {
    val plot1 = plotOne("1")
    val plot2 = plotOne("2")
    val plot3 = plotOne("3")
    val plot4 = plotOne("4")
    BokehUtils.multiplePlots(List(List(plot1, plot2), List(plot3, plot4)), "all chart")
  }

  def plotOne(title: String = ""): Plot = {
    val plot = BokehUtils.getPlot(xdr, ydr, Pan | WheelZoom | Crosshair)
    plotBasic(plot)
    val legend = plotContent(plot)
    plotLegend(plot, legend)
    plot.title(title)
  }

  def plotBasic(plot: Plot) = {
    val xaxis = BokehUtils.getLinearAxis(plot, Location.Below)
    BokehUtils.setAxisLabel(xaxis, "x")
    val yaxis = BokehUtils.getLinearAxis(plot, Location.Right)
    BokehUtils.setAxisLabel(yaxis, "y")
    val xgrid = BokehUtils.getGrid(plot, xaxis, 0)
    val ygrid = BokehUtils.getGrid(plot, yaxis, 1)
  }

  def plotContent(plot: Plot) = {
    val circleGlyph = BokehUtils.setCircleGlyph(plot, x, y, source)
    val lineGlyph = BokehUtils.setLineGlyph(plot, x, z, source)
    val lineGlyph2 = BokehUtils.setLineGlyph(plot, x, y, source)
    val patchGlyph = BokehUtils.setPatchGlyph(plot, x, p, source)
    val circleCrossGlyph = BokehUtils.setCircleCrossGlyph(plot, x, p, source)
    val textGlyph = BokehUtils.setTextGlyph(plot, x, z, source)
    List("y = sin(x)" -> List(circleGlyph, lineGlyph2), "y = x^2" -> List(lineGlyph), "y = x^3" -> List(circleCrossGlyph, patchGlyph))
  }

  def plotLegend(plot: Plot, legends: List[(String, List[GlyphRenderer])]) = {
    BokehUtils.getLegends(plot, legends)
  }
}
