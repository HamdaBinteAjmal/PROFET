/* 
 * PROFET Copyright 2015 (c) Data Mining and Machine Learning Group,
 * National University of Ireland Galway.  
 * This file is a part of PROFET  
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package Nodes;

/**
 *
 * @author Administrator
 */
public class Constant  implements java.io.Serializable {
    private String name;
    private double value = 0.0;
    public Constant(String name)
    {
        this.name = name;
    }
    public Constant(String name, double value)
    {
        this.name = name;
        this.value = value;
    }
    @Override
    public String toString()
    {
        return this.name;
    }
    public void SetValue(double val)
    {
        this.value = val;
    }
    public double GetValue()
    {
        return this.value;
    }
}
