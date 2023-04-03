import styles from "../styles/Home.module.css";
import { initializeConnector } from "@web3-react/core";
import { Network } from "@web3-react/network";
import { useEffect } from "react";
import Chain, { URLS } from "./Chain";

export const [network, hooks] = initializeConnector<Network>(
  (actions) => new Network(actions, URLS),
  Object.keys(URLS).map((chainId) => Number(chainId))
);

const {
  useChainId,
  useAccounts,
  useError,
  useIsActivating,
  useIsActive,
  useProvider,
  useENSNames,
} = hooks;

export default function NetworkStatus() {
  const chainId = useChainId();
  const accounts = useAccounts();
  const error = useError();
  const isActivating = useIsActivating();

  const isActive = useIsActive();

  const provider = useProvider();
  const ENSNames = useENSNames(provider);

  // console.log("ensnames network", ENSNames);

  // attempt to connect eagerly on mount
  useEffect(() => {
    void network.activate();
  }, []);

  return (
    <div className={styles.card}>
      <h2>Network Status</h2>

      <table>
        <tbody>
          <tr>
            <th>Chain</th>
            <td>
              <Chain chainId={chainId} />
            </td>
          </tr>
          <tr>
            <th>Accounts</th>
            <td>{accounts}</td>
          </tr>
          <tr>
            <th>isActivating</th>
            <td>{isActivating ? "true" : "false"}</td>
          </tr>
          <tr>
            <th>isActive</th>
            <td>{isActive ? "true" : "false"}</td>
          </tr>
        </tbody>
      </table>
    </div>
  );
}
