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
package anthology

import anticipation.*
import gossamer.*
import vacuous.*

// Builds the `Axml.Element` tree for an Android manifest. The resource IDs below are the stable,
// public `android.R.attr.*` constants (verified against `aapt`'s own output); public resource
// IDs never change across platform versions, so hardcoding the handful a manifest needs avoids
// depending on the platform `android.jar`'s resource table at all.
object ApkManifest:
  private val versionCode:      Int = 0x0101021b
  private val versionName:      Int = 0x0101021c
  private val minSdkVersion:    Int = 0x0101020c
  private val targetSdkVersion: Int = 0x01010270
  private val nameAttr:         Int = 0x01010003
  private val labelAttr:        Int = 0x01010001
  private val exportedAttr:     Int = 0x01010010

  private def android(name: Text, id: Int, value: Axml.Value): Axml.Attribute =
    Axml.Attribute(Axml.androidUri, name, id, value)

  def apply
    ( packageName:  Text,
      versionCode:  Int,
      versionName:  Text,
      minSdk:       Int,
      targetSdk:    Int,
      label:        Text,
      activity:     Text,
      permissions:  List[Text] )
  :   Axml.Element =

    // Each requested runtime permission is a `<uses-permission android:name="…"/>` element.
    val permissionElements = permissions.map: permission =>
      Axml.Element
        ( t"uses-permission",
          List(android(t"name", nameAttr, Axml.Value.Str(permission))),
          Nil )

    val launcher =
      Axml.Element
        ( t"intent-filter",
          Nil,
          List
            ( Axml.Element
                ( t"action",
                  List(android(t"name", nameAttr,
                      Axml.Value.Str(t"android.intent.action.MAIN"))),
                  Nil ),
              Axml.Element
                ( t"category",
                  List(android(t"name", nameAttr,
                      Axml.Value.Str(t"android.intent.category.LAUNCHER"))),
                  Nil ) ) )

    val activityElement =
      Axml.Element
        ( t"activity",
          List
            ( android(t"name", nameAttr, Axml.Value.Str(activity)),
              android(t"exported", exportedAttr, Axml.Value.Bool(true)) ),
          List(launcher) )

    val application =
      Axml.Element
        ( t"application",
          List(android(t"label", labelAttr, Axml.Value.Str(label))),
          List(activityElement) )

    val usesSdk =
      Axml.Element
        ( t"uses-sdk",
          List
            ( android(t"minSdkVersion", minSdkVersion, Axml.Value.Num(minSdk)),
              android(t"targetSdkVersion", targetSdkVersion, Axml.Value.Num(targetSdk)) ),
          Nil )

    Axml.Element
      ( t"manifest",
        List
          ( Axml.Attribute(Unset, t"package", Unset, Axml.Value.Str(packageName)),
            android(t"versionCode", ApkManifest.versionCode, Axml.Value.Num(versionCode)),
            android(t"versionName", ApkManifest.versionName, Axml.Value.Str(versionName)) ),
        usesSdk :: permissionElements ++ List(application) )
