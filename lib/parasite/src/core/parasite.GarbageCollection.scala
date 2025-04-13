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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package parasite

import language.experimental.into
import language.experimental.pureFunctions

import java.lang.ref as jlr
import java.lang.management as jlm
import javax.management as jm
import javax.management.openmbean as jmo
import com.sun.management as csm

import anticipation.*
import denominative.*
import prepositional.*
import proscenium.*
import rudiments.*
import symbolism.*

object GarbageCollection:
  enum Collector:
    case G1YoungGeneration
    case G1OldGeneration
    case G1Concurrent
    case PsScavenge
    case PsMarkSweep
    case ParNew
    case ConcurrentMarkSweep
    case Copy
    case MarkSweepCompact
    case Other(name: Text)

  object Collector:
    def unapply(name: Text): Some[Collector] = name.s match
      case "G1 Young Generation" => Some(G1YoungGeneration)
      case "G1 Old Generation"   => Some(G1OldGeneration)
      case "G1 Concurrent GC"    => Some(G1Concurrent)
      case "PS Scavenge"         => Some(PsScavenge)
      case "PS MarkSweep"        => Some(PsMarkSweep)
      case "ParNew"              => Some(ParNew)
      case "ConcurrentMarkSweep" => Some(ConcurrentMarkSweep)
      case "Copy"                => Some(Copy)
      case "MarkSweepCompact"    => Some(MarkSweepCompact)
      case other                 => Some(Other(name))

  enum Cause:
    case AllocationFailure, SystemGc, GcLocker, Metadata, Ergonomics, CmsInitialMark,
         CmsFinalRemark, FullGc, HeapInspection, NoGc, G1EvacuationPause, G1HumongousAllocation
    case Other(cause: Text)

  object Cause:
    def unapply(text: Text): Some[Cause] = text.s match
      case "Allocation Failure"           => Some(AllocationFailure)
      case "System.gc()"                  => Some(SystemGc)
      case "GCLocker Initiated GC"        => Some(GcLocker)
      case "Metadata GC Threshold"        => Some(Metadata)
      case "Ergonomics"                   => Some(Ergonomics)
      case "CMS Initial Mark"             => Some(CmsInitialMark)
      case "CMS Final Remark"             => Some(CmsFinalRemark)
      case "Full GC"                      => Some(FullGc)
      case "Heap Inspection Initiated GC" => Some(HeapInspection)
      case "No GC"                        => Some(NoGc)
      case "G1 Evacuation Pause"          => Some(G1EvacuationPause)
      case "G1 Humongous Allocation"      => Some(G1HumongousAllocation)
      case other                          => Some(Other(text))

  given interceptable: GarbageCollection is Interceptable:
    type Target = System.type

    def register(value: System.type, action: GarbageCollection => Unit): () => Unit =
      val listeners =
        jlm.ManagementFactory.getGarbageCollectorMXBeans().nn.asScala.to(List).flatMap:
          case emitter: jm.NotificationEmitter =>
            val listener: jm.NotificationListener = (notification, handback) =>
              if notification.nn.getType()
                 == csm.GarbageCollectionNotificationInfo.GARBAGE_COLLECTION_NOTIFICATION
              then
                notification.nn.getUserData.nn match
                  case info: jmo.CompositeData =>
                    val gc = csm.GarbageCollectionNotificationInfo.from(info).nn
                    val Cause(cause) = gc.getGcCause().nn.tt
                    val Collector(collector) = gc.getGcName().nn.tt
                    val gcInfo = gc.getGcInfo().nn
                    val preMemory = gcInfo.getMemoryUsageBeforeGc().nn
                    val postMemory = gcInfo.getMemoryUsageAfterGc().nn

                    val memory: Map[Text, (Memory, Memory)] =
                      preMemory.keySet.nn.iterator.nn.asScala.map: key =>
                        key.tt
                        -> (preMemory.get(key).nn.getUsed.b, postMemory.get(key).nn.getUsed.b)
                      .to(Map)

                    action(GarbageCollection(gcInfo.getId.toInt.z, collector, cause, memory))

                  case _ =>
                    ()

            emitter.addNotificationListener(listener, null, null)

            List(emitter -> listener)

          case _ =>
            Nil

      () => listeners.each: (emitter, listener) =>
        emitter.removeNotificationListener(listener)

case class GarbageCollection
            (run:       Ordinal,
             collector: GarbageCollection.Collector,
             cause:     GarbageCollection.Cause,
             memory:    Map[Text, (before: Memory, after: Memory)]):
  def before: Memory = memory.to(List).map(_(1).before).total
  def after: Memory = memory.to(List).map(_(1).after).total
  def reduction: Memory = before - after
