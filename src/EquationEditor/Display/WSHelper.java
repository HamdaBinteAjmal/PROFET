/* 
 * Copyright (C) 2010 Alex Billingsley, email@alexbillingsley.co.uk
 * www.dragmath.bham.ac.uk 
 * This file is part of DragMath.
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



 
 

package EquationEditor.Display;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import sun.misc.BASE64Decoder;
import sun.misc.BASE64Encoder;

/**
 *
 * @author Alex Billingsley
 */
public class WSHelper {
    
    static private BASE64Encoder encode = new BASE64Encoder();
    static private BASE64Decoder decode = new BASE64Decoder();
    
    static public String OToS(Object obj) {
        
        long start=System.currentTimeMillis();
        String out = null;
        if (obj != null) {
            try {
                ByteArrayOutputStream baos = new ByteArrayOutputStream();
                ObjectOutputStream oos = new ObjectOutputStream(baos);
                oos.writeObject(obj);
                out = encode.encode(baos.toByteArray());
            } catch (IOException e) {
                e.printStackTrace();
                return null;
            }
        }
        
        long end=System.currentTimeMillis();
        System.out.println("Encode:"+(end-start));
        return out;
    }
    
    static public Object SToO(String str) {
        long start=System.currentTimeMillis();
        Object out = null;
        if (str != null) {
            try {
                ByteArrayInputStream bios = new
                        ByteArrayInputStream(decode.decodeBuffer(str));
                ObjectInputStream ois = new ObjectInputStream(bios);
                out = ois.readObject();
            } catch (IOException e) {
                e.printStackTrace();
                return null;
            } catch (ClassNotFoundException e) {
                e.printStackTrace();
                return null;
            }
        }
        long end=System.currentTimeMillis();
        System.out.println("Decode:"+(end-start));
        return out;
    }
}
