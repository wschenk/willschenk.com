import styles from "../styles/Home.module.css";
import { CoinbaseWallet } from "@web3-react/coinbase-wallet";
import { initializeConnector } from "@web3-react/core";
import { useEffect, useState } from "react";
import Chain, { URLS } from "./Chain";
import type { BigNumber } from "@ethersproject/bignumber";
import { formatUnits } from "@ethersproject/units";

export const [coinbaseWallet, hooks] = initializeConnector<CoinbaseWallet>(
  (actions) =>
    new CoinbaseWallet(actions, {
      url: URLS[1][0],
      appName: "web3-react",
    })
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

export default function CoinbaseWalletCard() {
  const chainId = useChainId();
  const accounts = useAccounts();
  const error = useError();
  const isActivating = useIsActivating();

  const isActive = useIsActive();

  const provider = useProvider();
  const ENSNames = useENSNames(provider);

  // attempt to connect eagerly on mount
  useEffect(() => {
    void coinbaseWallet.connectEagerly();
  }, []);

  // Balance

  const [balances, setBalances] = useState<BigNumber[] | undefined>(undefined);

  useEffect(() => {
    if (provider && accounts) {
      void Promise.all(
        accounts.map((account) => provider.getBalance(account))
      ).then((balances) => {
        // if (stale) return
        setBalances(balances);
      });
    }
  }, [provider, accounts]);

  return (
    <div className={styles.card}>
      <h2>Coinbase Wallet</h2>
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

          <tr>
            <th>ENS Names</th>
            <td>{ENSNames}</td>
          </tr>

          {balances &&
            accounts &&
            accounts?.map((account, i) => (
              <tr key={account}>
                <th>Account {i}</th>
                <td>{formatUnits(balances[i], 18)}</td>
              </tr>
            ))}
        </tbody>
      </table>

      {!isActive && !isActivating && (
        <button onClick={() => coinbaseWallet.activate()}>Connect</button>
      )}

      {isActive && !isActivating && (
        <button onClick={() => coinbaseWallet.deactivate()}>Disconnect</button>
      )}
    </div>
  );
}
