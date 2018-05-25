
import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Props;
import akka.actor.UntypedActor;

import java.io.IOException;
import java.io.Serializable;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by irilu on 12/1/2016.
 */
// -- MESSAGES --------------------------------------------------
class SubscribeMessage implements Serializable {
    public final ActorRef subscriber;
    public SubscribeMessage(ActorRef subscriber) {
        this.subscriber = subscriber;
    }
}
class UnsubscribeMessage implements Serializable {
    public final ActorRef unsubscriber;
    public UnsubscribeMessage(ActorRef unsubscriber) {
        this.unsubscriber = unsubscriber;
    }
}
class Message implements Serializable {
   public final String s;
   public Message(String s) {
       this.s = s;
   }
}

// -- ACTORS --------------------------------------------------
class BroadcastActor extends UntypedActor {
    private List<ActorRef> list =
            new ArrayList<ActorRef>();
    public void onReceive(Object o) throws Exception {
        if (o instanceof SubscribeMessage) {
            list.add(((SubscribeMessage) o).subscriber);
        } else if (o instanceof UnsubscribeMessage) {
            list.remove(((UnsubscribeMessage) o).unsubscriber);
        } else if (o instanceof Message) {
            for (ActorRef person : list) {
                person.tell(o, getSelf());
            }
        }
    }
}
class PersonActor extends UntypedActor {
   public void onReceive(Object o) throws Exception {
       if (o instanceof Message) {
           System.out.println(((Message) o).s);
       }
   }
}

// -- MAIN --------------------------------------------------
public class Broadcast {
    public static void main(String[] args) {
        final ActorSystem system =
                ActorSystem.create("EccoSystem");
        final ActorRef broadcaster =
                system.actorOf(Props.create(BroadcastActor.class), "broadcaster");
        final ActorRef p1 = system.actorOf(Props.create(PersonActor.class), "p1");
        final ActorRef p2 = system.actorOf(Props.create(PersonActor.class), "p2");
        final ActorRef p3 = system.actorOf(Props.create(PersonActor.class), "p3");
        broadcaster.tell(new SubscribeMessage(p1), ActorRef.noSender());
        broadcaster.tell(new SubscribeMessage(p2), ActorRef.noSender());
        broadcaster.tell(new SubscribeMessage(p3), ActorRef.noSender());
        broadcaster.tell(new Message("purses half price!"), ActorRef.noSender());
        broadcaster.tell(new UnsubscribeMessage(p2), ActorRef.noSender());
        broadcaster.tell(new Message("shoes half price!!"), ActorRef.noSender());
        try {
            System.out.println("Press return to terminate...");
            System.in.read();
        } catch(IOException e) {
            e.printStackTrace();
        } finally {
            system.shutdown();
        }
    }
}
