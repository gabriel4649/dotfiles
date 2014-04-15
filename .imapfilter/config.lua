account = IMAP {
  server = 'neomailbox.net',
  username = 'gabriel.perez',
  password = 'F<D:~R<k',
  ssl = 'ssl3'
}

gatech_messages =
   account.INBOX:contain_from("atira.rochester@gatech.edu") +
   account.INBOX:contain_from("wmerkousko3@mail.gatech.edu") +
   account.INBOX:contain_from("notifications@passport.gatech.edu") +
   account.INBOX:contain_from("noreply@oit.gatech.edu") +
   account.INBOX:contain_from("raczynski@cc.gatech.edu") +
   account.INBOX:contain_from("sonia.anderson@success.gatech.edu") +
   account.INBOX:contain_from("ms-cs-official@cc.gatech.edu") +
   account.INBOX:contain_from("amerchant6@gatech.edu") +
   account.INBOX:contain_from("finaid@gatech.edu") +
   account.INBOX:contain_from("STickets@gtaa.gatech.edu") +
   account.INBOX:contain_from("communications@cc.gatech.edu") +
   account.INBOX:contain_from("andrea.fekete@success.gatech.edu") +
   account.INBOX:contain_from("gtpresident@gatech.edu") +
   account.INBOX:contain_from("gatech@careereco.com") +
   account.INBOX:contain_from("andrea.comsa@success.gatech.edu") +
   account.INBOX:contain_from("announcements@buzzport.gatech.edu") +
   account.INBOX:contain_from("melissa.moore@gatech.edu") +
   account.INBOX:contain_from("sonia.mcallister@success.gatech.edu") +
   account.INBOX:contain_from("studentevents-noreply@vpss.gatech.edu") +
   account.INBOX:contain_from("training@osp.gatech.edu") +
   account.INBOX:contain_from("gatech@csm.symplicity.com") +
   account.INBOX:contain_from("galil@cc.gatech.edu") +
   account.INBOX:contain_from("kevin.stacia@success.gatech.edu") +
   account.INBOX:contain_from("gwendolynn.kenny@dopp.gatech.edu") +
   account.INBOX:contain_from("marge.dussich@success.gatech.edu")

gatech_messages:move_messages(account['GaTech'])

messages_to_delete =
   account.INBOX:contain_from("amnistiapr.medios@gmail.com") +
   account.INBOX:contain_from("s_amp@rcse.upr.edu") +
   account.INBOX:contain_from("gsnews@gamestop-email.com") +
   account.INBOX:contain_from("df-teams@documentfreedom.org") +
   account.INBOX:contain_from("cruzpol@ece.uprm.edu") +
   account.INBOX:contain_from("gpucuda-owner@ece.uprm.edu") +
   account.INBOX:contain_from("gpucuda-bounces@ece.uprm.edu") +
   account.INBOX:contain_from("techcommunity@opensourceecology.org") +
   account.INBOX:contain_from("marcin@opensourceecology.org") +
   account.INBOX:contain_from("info@mightytext.net") +
   account.INBOX:contain_from("noreply@prsn.uprm.edu") +
   account.INBOX:contain_from("salomon.herrera@ieee.org") +
   account.INBOX:contain_from("computer.society.uprm@gmail.com") +
   account.INBOX:contain_from("campusverde@uprm.edu") +
   account.INBOX:contain_from("ieeeservice@ieee.org") +
   account.INBOX:contain_from("j.gutierrez@ieee.org") +
   account.INBOX:contain_from("raulmaxcemi@gmail.com") +
   account.INBOX:contain_from("cssieee@gmail.com") +
   account.INBOX:contain_from("gold@ieee.org") +
   account.INBOX:contain_from("rrr2009@ieee.org") +
   account.INBOX:contain_from("share@push.pickensplan.com") +
   account.INBOX:contain_from("owner-ieee-e-notice@ieee.org") +
   account.INBOX:contain_from("mst.mayaguez@gmail.com") +
   account.INBOX:contain_from("conference-services@ieee.org") +
   account.INBOX:contain_from("marilomf@gmail.com") +
   account.INBOX:contain_from("simone.darby@ieee.org") +
   account.INBOX:contain_from("visibility@ieee.org") +
   account.INBOX:contain_from("csconnection@computer.org") +
   account.INBOX:contain_from("ieeemembers.ece.uprm.edu") +
   account.INBOX:contain_from("fmoroni@ieee.org") +
   account.INBOX:contain_from("the-institute@ieee.org") +
   account.INBOX:contain_from("capacitacion@top-formacion.org.ar") +
   account.INBOX:contain_from("vsimons@ieee.org") +
   account.INBOX:contain_from("ieee_prc@gmx.com") +
   account.INBOX:contain_from("rew@reworld-media.com") +
   account.INBOX:contain_from("jw.daniel@computer.org") +
   account.INBOX:contain_from("store-news@amazon.com") +
   account.INBOX:contain_from("techinsider@ieee.org") +
   account.INBOX:contain_from("gsnews@gamestop-email.com") +
   account.INBOX:match_from(".*@facebookmail.com")

account.INBOX:delete_messages(messages_to_delete)
