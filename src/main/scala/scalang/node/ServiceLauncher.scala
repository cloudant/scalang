//
// Copyright 2012, Boundary
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
package scalang.node

import scalang._

class ServiceLauncher[A <: Product, T <: Process](clazz : Class[T], ctx : ServiceContext[A]) extends ProcessHolder(ctx) {
  val referenceCounter = ctx.referenceCounter
  var process : Process = null

  def init {
    val constructor = clazz.getConstructor(classOf[ServiceContext[_]])
    ctx.adapter = this
    process = constructor.newInstance(ctx)
  }
}
