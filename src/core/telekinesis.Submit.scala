/*
    Telekinesis, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package telekinesis

import language.dynamics

import scala.quoted.*

import anticipation.*
import nettlesome.*
import prepositional.*
import proscenium.*

case class Submit[TargetType](url: HttpUrl, target: TargetType) extends Dynamic:
  inline def applyDynamicNamed[PayloadType]
     (id: "apply")
     (inline headers: (Label, Any)*)
     (payload: PayloadType)
     (using online:   Online,
            loggable: HttpEvent is Loggable,
            postable: PayloadType is Postable,
            client:   HttpClient onto TargetType)
  :     HttpResponse =

    ${
        Telekinesis.submit[TargetType, PayloadType]
         ('this, 'headers, 'online, 'loggable, 'payload, 'postable, 'client)  }

  inline def applyDynamic[PayloadType: Postable as postable](id: "apply")(inline headers: Any*)
     (payload: PayloadType)
     (using online:   Online,
            loggable: HttpEvent is Loggable,
            client:   HttpClient onto TargetType)
  :     HttpResponse =

    ${
        Telekinesis.submit[TargetType, PayloadType]
         ('this, 'headers, 'online, 'loggable, 'payload, 'postable, 'client)  }
