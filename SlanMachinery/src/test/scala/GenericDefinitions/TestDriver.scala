/*
 * Copyright (c) 7/12/24, 10:52 AM, 12. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package GenericDefinitions

import org.scalatest.Suites
class TestDriver extends Suites (new AgentEntityTest,new ResourceEntityTest, new GroupEntityTest, new MessageEntityTest, new BehaviorEntityTest, new AgentEntityTest, new ResourceEntityTest, new GroupEntityTest, new MessageEntityTest, new BehaviorEntityTest)
