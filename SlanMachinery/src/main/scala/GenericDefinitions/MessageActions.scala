/*
 * Copyright (c) 8/2/24, 10:38 AM, 2. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *  
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

def receive(m: MessageEntity): MessageActions = new MessageActions(m)

class MessageActions(m: MessageEntity):
  infix def to(c: ChannelEntity): Unit = println(s"Sending message $m to channel $c")
/*
  infix def from(c: ChannelEntity): MessageCallback = println(s"Sending message $m to channel $c")
  infix def process(cb: => Unit): Unit = println(s"Receiving message $m from channel $c")
*/
  
