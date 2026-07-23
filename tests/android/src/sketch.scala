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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package sketch

import soundness.*

import android.app.{Activity, AlertDialog}
import android.content.{Context, SharedPreferences}
import android.graphics.{Canvas, Color, Paint, Path}
import android.view.{Gravity, MotionEvent, View, ViewGroup}
import android.widget.{Button, LinearLayout, TextView, Toast}

// A finger-painting app, exercising a broad slice of the Android API through xenophile's
// facades: a custom `View` subclass drawing a `Path` with a `Paint` onto the `Canvas` the
// framework hands it, touch handling from `MotionEvent`, an `AlertDialog` colour picker (whose
// item listener is an arity-2 functional interface), `SharedPreferences` persistence of the
// chosen colour, and a scattering of static methods and constants. Nothing here is written
// against a raw Java signature — every Android call goes through the facade layer.
class SketchActivity extends Activity:
  private val palette: List[(Text, Int)] =
    List(t"Black" -> Color.BLACK, t"Red" -> Color.RED, t"Green" -> Color.GREEN,
        t"Blue" -> Color.BLUE, t"Magenta" -> Color.MAGENTA)

  override def onCreate(saved: android.os.Bundle): Unit =
    super.onCreate(saved)
    val activity = Facade(this: Activity)

    val prefs = Facade(getSharedPreferences(t"sketch".s, Context.MODE_PRIVATE))
    val savedColor: Int = prefs.getInt(t"color", Color.BLACK)

    val canvas = SketchView(this)
    canvas.color(savedColor)

    val clear = make[Button](this)
    clear.setText(t"Clear")
    clear.setOnClickListener(view => canvas.clear())

    val color = make[Button](this)
    color.setText(t"Colour")

    color.setOnClickListener: view =>
      // A Scala `Array[Text]` now bridges to the `CharSequence[]` parameter.
      val names: Array[Text] = palette.map(_(0)).toArray
      make[AlertDialog.Builder](this)
      . setTitle(t"Choose a colour")
      . setItems(names, (dialog: android.content.DialogInterface, which: Int) => {
          val chosen = palette(which)(1)
          canvas.color(chosen)
          // `Editor.apply()` collides with the facade's index-access operator (`facade(args)`
          // desugars to `facade.apply(args)`), so it is reached on the unwrapped value.
          Facade(prefs.k.edit()).putInt(t"color", chosen).k.apply()
          Toast.makeText(this, t"Colour set".s, Toast.LENGTH_SHORT).show()
        })
      . show()

      ()

    val buttons = make[LinearLayout](this)
    buttons.setOrientation(LinearLayout.HORIZONTAL)
    buttons.addView(clear)
    buttons.addView(color)

    val layout = make[LinearLayout](this)
    layout.setOrientation(LinearLayout.VERTICAL)

    // A weighted layout parameter so the canvas fills the space above the buttons.
    val fill = make[LinearLayout.LayoutParams](ViewGroup.LayoutParams.MATCH_PARENT, 0, 1.0f)
    layout.addView(canvas, fill.k)
    layout.addView(buttons)

    activity.setContentView(layout)

// The drawing surface: accumulates finger strokes into a `Path` and repaints.
class SketchView(context: Context) extends View(context):
  private val path = make[Path]()
  private val brush = make[Paint]()

  brush.setStyle(Paint.Style.STROKE)
  brush.setStrokeWidth(12)               // an Int, widened to the `float` parameter
  brush.setStrokeCap(Paint.Cap.ROUND)
  brush.setStrokeJoin(Paint.Join.ROUND)
  brush.setAntiAlias(true)

  def color(value: Int): Unit = brush.color = value

  def clear(): Unit =
    path.reset()
    invalidate()

  override def onDraw(surface: Canvas): Unit =
    val facade = Facade(surface)
    facade.drawColor(Color.WHITE)
    facade.drawPath(path.k, brush.k)

  override def onTouchEvent(event: MotionEvent): Boolean =
    val touch = Facade(event)
    val x: Float = touch.x
    val y: Float = touch.y

    touch.action match
      case MotionEvent.ACTION_DOWN => path.moveTo(x, y)
      case MotionEvent.ACTION_MOVE => path.lineTo(x, y)
      case _                       => ()

    invalidate()
    true
